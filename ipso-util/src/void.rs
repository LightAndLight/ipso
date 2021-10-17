pub enum Void {}

impl Void {
    pub fn absurd<A>(&self) -> A {
        panic!("void")
    }
}
