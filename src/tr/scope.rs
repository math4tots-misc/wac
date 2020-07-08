use super::*;

pub(super) enum Scope {
    Local,
    Global,
}

impl fmt::Display for Scope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Scope::Local => write!(f, "local"),
            Scope::Global => write!(f, "global"),
        }
    }
}

pub(super) struct GlobalScope {
    pub(super) functions: HashMap<Rc<str>, FunctionEntry>,
    pub(super) varmap: HashMap<Rc<str>, ScopeEntry>,
    pub(super) decls: Vec<Rc<GlobalVarInfo>>,
    #[allow(dead_code)]
    pub(super) traits_by_id: Vec<Rc<TraitInfo>>,

    /// Maps (receiver type) => (trait id) => (ImplInfo)
    pub(super) impls_map: HashMap<Type, HashMap<i32, Rc<ImplInfo>>>,

    /// all impls declared in this scope
    pub(super) impls: Vec<Rc<ImplInfo>>,
}

impl GlobalScope {
    pub(super) fn new(
        functions: HashMap<Rc<str>, FunctionEntry>,
        traits_by_id: Vec<Rc<TraitInfo>>,
    ) -> Self {
        Self {
            functions,
            varmap: HashMap::new(),
            decls: vec![],
            traits_by_id,
            impls_map: HashMap::new(),
            impls: Vec::new(),
        }
    }

    pub(super) fn decl_const(
        &mut self,
        span: SSpan,
        name: Rc<str>,
        cval: ConstValue,
    ) -> Result<Rc<ConstantInfo>, Error> {
        if let Some(info) = self.varmap.get(&name) {
            return Err(Error::ConflictingDefinitions {
                span1: info.span().clone(),
                span2: span,
                name,
            });
        }
        let info = Rc::new(ConstantInfo {
            span,
            name: name.clone(),
            value: cval,
        });
        self.varmap
            .insert(name.clone(), ScopeEntry::Constant(info.clone()));
        Ok(info)
    }

    pub(super) fn decl_gvar(
        &mut self,
        span: SSpan,
        name: Rc<str>,
        type_: Type,
    ) -> Result<Rc<GlobalVarInfo>, Error> {
        if let Some(info) = self.varmap.get(&name) {
            return Err(Error::ConflictingDefinitions {
                span1: info.span().clone(),
                span2: span,
                name,
            });
        }
        let wasm_name = format!("$g_{}", name).into();
        let info = Rc::new(GlobalVarInfo {
            span,
            original_name: name.clone(),
            type_,
            wasm_name,
        });
        self.decls.push(info.clone());
        self.varmap
            .insert(name.clone(), ScopeEntry::Global(info.clone()));
        Ok(info)
    }

    pub(super) fn get_trait(
        &mut self,
        span: &SSpan,
        name: &Rc<str>,
    ) -> Result<&Rc<TraitInfo>, Error> {
        match self.functions.get(name) {
            Some(FunctionEntry::Trait(info)) => Ok(info),
            Some(_) => Err(Error::Type {
                span: span.clone(),
                expected: "trait".into(),
                got: "function".into(),
            }),
            None => Err(Error::Type {
                span: span.clone(),
                expected: "trait".into(),
                got: "not found".into(),
            }),
        }
    }

    pub(super) fn decl_impl(
        &mut self,
        span: SSpan,
        fname: Rc<str>,
        receiver_type: Type,
        trait_: Rc<TraitInfo>,
    ) -> Result<Rc<ImplInfo>, Error> {
        let index = self.impls.len() as i32;
        let info = Rc::new(ImplInfo {
            span: span.clone(),
            type_: receiver_type,
            trait_: trait_.clone(),
            fname,
            index,
        });
        let map = self.impls_map.entry(receiver_type).or_default();
        match map.get(&trait_.id) {
            Some(old_info) => {
                return Err(Error::ConflictingDefinitions {
                    span1: old_info.span.clone(),
                    span2: info.span.clone(),
                    name: format!("impl {} {}", receiver_type, trait_.name).into(),
                })
            }
            None => {
                map.insert(trait_.id, info.clone());
            }
        }
        self.impls.push(info.clone());
        Ok(info)
    }

    pub(super) fn get_impl(&self, receiver_type: Type, trait_id: i32) -> Option<Rc<ImplInfo>> {
        match self.impls_map.get(&receiver_type).and_then(|map| map.get(&trait_id)) {
            Some(info) => Some(info.clone()),
            None => {
                // if there's no exact match, check to see if there is an 'id' impl
                self.impls_map.get(&Type::Id).and_then(|map| map.get(&trait_id)).cloned()
            }
        }
    }
}

pub(super) struct ConstantInfo {
    pub(super) span: SSpan,
    #[allow(dead_code)]
    pub(super) name: Rc<str>,
    pub(super) value: ConstValue,
}

/// global variable declaration
pub(super) struct GlobalVarInfo {
    #[allow(dead_code)]
    pub(super) span: SSpan,
    #[allow(dead_code)]
    pub(super) original_name: Rc<str>,
    pub(super) type_: Type,
    pub(super) wasm_name: Rc<str>,
}

/// local variable declaration
pub(super) struct LocalVarInfo {
    #[allow(dead_code)]
    pub(super) span: SSpan,

    /// the programmer provided name for this variable
    pub(super) original_name: Rc<str>,

    pub(super) type_: Type,

    pub(super) wasm_name: Rc<str>,
}

pub(super) struct LocalScope<'a> {
    pub(super) g: &'a GlobalScope,
    pub(super) trace: bool,
    pub(super) locals: Vec<HashMap<Rc<str>, Rc<LocalVarInfo>>>,
    pub(super) nlabels: usize,
    pub(super) continue_labels: Vec<u32>,
    pub(super) break_labels: Vec<u32>,
    pub(super) decls: Vec<Rc<LocalVarInfo>>,

    /// local variables not directly created by the end-user
    /// but by the system as needed
    pub(super) helper_locals: HashMap<Rc<str>, Type>,

    next_unique_helper_id: usize,
}

impl<'a> LocalScope<'a> {
    pub(super) fn new(g: &'a GlobalScope, trace: bool) -> Self {
        Self {
            g,
            trace,
            locals: vec![HashMap::new()],
            nlabels: 0,
            continue_labels: vec![],
            break_labels: vec![],
            decls: vec![],
            helper_locals: HashMap::new(),
            next_unique_helper_id: 0,
        }
    }
    pub(super) fn helper_shared(&mut self, name: &str, type_: Type) {
        // it should be ok, but just to be extra safe, only allow
        // primitive shared values
        assert!(type_.primitive());

        assert!(name.starts_with("$rt_"));
        if let Some(old_type) = self.helper_locals.get(name) {
            assert_eq!(*old_type, type_);
        }
        self.helper_locals.insert(name.into(), type_);
    }
    pub(super) fn helper_unique(&mut self, type_: Type) -> Rc<str> {
        // it should be ok, but just to be extra safe, only allow
        // primitive shared values
        assert!(type_.primitive());

        let name: Rc<str> = format!("$rtu_{}", self.next_unique_helper_id).into();
        self.next_unique_helper_id += 1;
        if let Some(old_type) = self.helper_locals.get(&name) {
            assert_eq!(*old_type, type_);
        }
        self.helper_locals.insert(name.clone(), type_);
        name
    }
    pub(super) fn push(&mut self) {
        self.locals.push(HashMap::new());
    }
    pub(super) fn pop(&mut self) {
        self.locals.pop().unwrap();
    }
    pub(super) fn decl(
        &mut self,
        span: SSpan,
        original_name: Rc<str>,
        type_: Type,
    ) -> Rc<LocalVarInfo> {
        let id = self.decls.len();
        let wasm_name = format!("$l_{}_{}", id, original_name).into();
        let info = Rc::new(LocalVarInfo {
            span,
            original_name,
            type_,
            wasm_name,
        });
        self.decls.push(info.clone());
        self.locals
            .last_mut()
            .unwrap()
            .insert(info.original_name.clone(), info.clone());
        info
    }
    pub(super) fn get(&self, name: &Rc<str>) -> Option<ScopeEntry> {
        for map in self.locals.iter().rev() {
            match map.get(name) {
                Some(t) => return Some(ScopeEntry::Local(t.clone())),
                None => {}
            }
        }
        match self.g.varmap.get(name) {
            Some(t) => Some(t.clone()),
            None => None,
        }
    }
    pub(super) fn get_or_err(&self, span: SSpan, name: &Rc<str>) -> Result<ScopeEntry, Error> {
        match self.get(name) {
            Some(e) => Ok(e),
            None => Err(Error::Type {
                span,
                expected: format!("Variable {}", name),
                got: "NotFound".into(),
            }),
        }
    }
    pub(super) fn getf(&self, name: &Rc<str>) -> Option<FunctionEntry> {
        self.g.functions.get(name).cloned()
    }
    pub(super) fn getf_or_err(&self, span: SSpan, name: &Rc<str>) -> Result<FunctionEntry, Error> {
        match self.getf(name) {
            Some(e) => Ok(e),
            None => Err(Error::Type {
                span,
                expected: format!("Function {}", name),
                got: "NotFound".into(),
            }),
        }
    }
    pub(super) fn new_label_id(&mut self) -> u32 {
        let id = self.nlabels as u32;
        self.nlabels += 1;
        id
    }
}

#[derive(Clone)]
pub(super) enum ScopeEntry {
    Local(Rc<LocalVarInfo>),
    Global(Rc<GlobalVarInfo>),
    Constant(Rc<ConstantInfo>),
}

impl ScopeEntry {
    pub(super) fn span(&self) -> &SSpan {
        match self {
            ScopeEntry::Local(info) => &info.span,
            ScopeEntry::Global(info) => &info.span,
            ScopeEntry::Constant(info) => &info.span,
        }
    }

    pub(super) fn type_(&self) -> Type {
        match self {
            ScopeEntry::Local(info) => info.type_,
            ScopeEntry::Global(info) => info.type_,
            ScopeEntry::Constant(info) => info.value.type_(),
        }
    }
}

#[derive(Clone)]
pub(super) enum FunctionEntry {
    Function(Rc<FunctionInfo>),
    Trait(Rc<TraitInfo>),
}

impl FunctionEntry {
    pub(super) fn type_(&self) -> &FunctionType {
        match self {
            FunctionEntry::Function(info) => &info.type_,
            FunctionEntry::Trait(info) => &info.type_,
        }
    }
}

pub(super) struct FunctionInfo {
    pub(super) type_: FunctionType,
    pub(super) name: Rc<str>,
}

pub(super) struct TraitInfo {
    pub(super) type_: FunctionType,
    pub(super) id: i32,
    pub(super) name: Rc<str>,
}

pub(super) struct ImplInfo {
    pub(super) span: SSpan,

    // the function name (excluding the $f_ prefix)
    pub(super) fname: Rc<str>,

    // the receiver type
    #[allow(dead_code)]
    pub(super) type_: Type,

    // the trait that this impl implements
    #[allow(dead_code)]
    pub(super) trait_: Rc<TraitInfo>,

    // index into the webassembly table
    // this is what you need to pass to call_indirect
    // to call this function
    pub(super) index: i32,
}
