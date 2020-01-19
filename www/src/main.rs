#[macro_use]
extern crate stdweb;

extern crate boolean;

#[js_export]
fn eval_str(x: String) -> String {
    boolean::eval_str(x)
}

fn main() {
    stdweb::initialize();
    js! {
        console.log("boolean_www: initialized");
    }
}
