pub fn make_import_object() -> wr::ImportObject {
    wr::imports! {
        "lang" => {
            "hello" => wr::func!(|_: &mut wr::Ctx| {
                println!("hello");
            }),
            "print_i32" => wr::func!(|_: &mut wr::Ctx, i: i32| {
                println!("{}", i);
            }),
        }
    }
}
