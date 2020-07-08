use crate::TAG_ID;
use crate::Type;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

thread_local! {
    /// Ok, so this is pretty evil
    /// But there's an assumption baked in that Type values should
    /// always be able to get its name without outside context
    ///
    /// While that's ok for builtin types, for user defined types
    /// this isn't really possible. Eventually I should refactor this
    /// but for now, I'm just going to live with a global variable.
    ///
    /// TODO: undo this evil
    static TYPE_MAP: RefCell<Option<GlobalTypeInfo>> = RefCell::new(None);
}

pub(crate) fn set_global_typeinfo(tmap: GlobalTypeInfo) {
    TYPE_MAP.with(|cell| {
        cell.replace(Some(tmap))
    });
}

pub(crate) fn get_name_for_record_type_with_offset(offset: u16) -> Rc<str> {
    TYPE_MAP.with(|cell| {
        match cell.borrow().as_ref().unwrap().offset_to_record_name.get(offset as usize) {
            Some(name) => name.clone(),
            None => format!("[record:{}]", offset).into(),
        }
    })
}

pub(crate) fn get_name_for_enum_type_with_offset(offset: u16) -> Rc<str> {
    TYPE_MAP.with(|cell| {
        match cell.borrow().as_ref().unwrap().offset_to_enum_name.get(offset as usize) {
            Some(name) => name.clone(),
            None => format!("[enum:{}]", offset).into(),
        }
    })
}

pub(crate) fn get_user_defined_type_from_name(name: &str) -> Option<Type> {
    TYPE_MAP.with(|cell| {
        cell.borrow().as_ref().and_then(|info| info.get_type_from_name(name))
    })
}

pub(crate) fn get_enum_value_from_name(enum_type: Type, name: &str) -> Option<i32> {
    TYPE_MAP.with(|cell| {
        cell.borrow().as_ref().unwrap().get_enum_value_from_name(enum_type, name)
    })
}

pub(crate) fn get_tag_limit() -> i32 {
    TYPE_MAP.with(|cell| {
        cell.borrow().as_ref().unwrap().get_tag_limit()
    })
}

/// Contains the mapping between user defined type names and
/// their offsets
pub(crate) struct GlobalTypeInfo {
    offset_to_record_name: Vec<Rc<str>>,
    record_name_to_offset: HashMap<Rc<str>, u16>,
    offset_to_enum_name: Vec<Rc<str>>,
    enum_name_to_offset: HashMap<Rc<str>, u16>,
    enum_members: Vec<Vec<Rc<str>>>,
    str_to_enum_id_maps: Vec<HashMap<Rc<str>, i32>>,
}

impl GlobalTypeInfo {
    pub(crate) fn new() -> Self {
        Self {
            offset_to_record_name: vec![],
            record_name_to_offset: HashMap::new(),
            offset_to_enum_name: vec![],
            enum_name_to_offset: HashMap::new(),
            enum_members: vec![],
            str_to_enum_id_maps: vec![],
        }
    }

    pub(crate) fn decl_enum(&mut self, name: Rc<str>, members: Vec<Rc<str>>) {
        assert_eq!(self.offset_to_enum_name.len(), self.enum_name_to_offset.len());
        assert_eq!(self.offset_to_enum_name.len(), self.enum_members.len());
        assert_eq!(self.offset_to_enum_name.len(), self.str_to_enum_id_maps.len());
        let offset = self.enum_members.len() as u16;
        let mut str_to_enum_id = HashMap::new();
        for (i, member) in members.iter().enumerate() {
            str_to_enum_id.insert(member.clone(), i as i32);
        }
        self.str_to_enum_id_maps.push(str_to_enum_id);
        self.enum_members.push(members);
        self.offset_to_enum_name.push(name.clone());
        self.enum_name_to_offset.insert(name, offset);
    }

    pub(crate) fn decl_record(&mut self, name: Rc<str>) {
        assert_eq!(self.offset_to_record_name.len(), self.record_name_to_offset.len());
        let offset = self.record_name_to_offset.len() as u16;
        self.offset_to_record_name.push(name.clone());
        self.record_name_to_offset.insert(name, offset);
    }

    pub(crate) fn get_type_from_name(&self, name: &str) -> Option<Type> {
        match self.record_name_to_offset.get(name) {
            Some(offset) => Some(Type::Record(*offset)),
            None => match self.enum_name_to_offset.get(name) {
                Some(offset) => Some(Type::Enum(*offset)),
                None => None,
            }
        }
    }

    pub(crate) fn get_enum_value_from_name(&self, enum_type: Type, name: &str) -> Option<i32> {
        match enum_type {
            Type::Enum(offset) => {
                self.str_to_enum_id_maps[offset as usize].get(name).cloned()
            }
            _ => panic!("get_enum_value_from_name, expected enum, got {}", enum_type),
        }
    }

    /// all tag values will be < this limit
    pub(crate) fn get_tag_limit(&self) -> i32 {
        let record_offset_limit = self.record_name_to_offset.len() as u16;
        let enum_offset_limit = self.enum_name_to_offset.len() as u16;

        let mut limit = TAG_ID + 1;

        if record_offset_limit > 0 {
            limit = std::cmp::max(limit, Type::Record(record_offset_limit - 1).tag());
        }
        if enum_offset_limit > 0 {
            limit = std::cmp::max(limit, Type::Record(enum_offset_limit - 1).tag());
        }

        limit
    }
}
