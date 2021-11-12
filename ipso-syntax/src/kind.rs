use std::rc::Rc;

use ipso_util::iter::Step;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum KindCompound {
    Arrow(Kind, Kind),
}

impl KindCompound {
    pub fn mk_arrow(a: Kind, b: Kind) -> Self {
        KindCompound::Arrow(a, b)
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Kind {
    Type,
    Row,
    Constraint,
    Meta(usize),
    Ref(Rc<KindCompound>),
}

impl Kind {
    pub fn iter_metas(&self) -> KindIterMetas {
        KindIterMetas::One(self)
    }

    pub fn mk_arrow(a: Kind, b: Kind) -> Self {
        Kind::Ref(Rc::new(KindCompound::Arrow(a, b)))
    }

    pub fn render(&self) -> String {
        match self {
            Kind::Ref(kind) => match kind.as_ref() {
                KindCompound::Arrow(a, b) => {
                    let mut val = String::new();
                    if a.is_arrow() {
                        val.push('(')
                    }
                    val.push_str(a.render().as_str());
                    if a.is_arrow() {
                        val.push(')')
                    }
                    val.push_str(" -> ");
                    val.push_str(b.render().as_str());
                    val
                }
            },
            Kind::Type => String::from("Type"),
            Kind::Row => String::from("Row"),
            Kind::Constraint => String::from("Constraint"),
            Kind::Meta(n) => format!("?{}", n),
        }
    }

    pub fn is_arrow(&self) -> bool {
        match self {
            Kind::Ref(kind) => matches!(kind.as_ref(), KindCompound::Arrow(_, _)),
            _ => false,
        }
    }
}

pub enum KindIterMetas<'a> {
    Zero,
    One(&'a Kind),
    Many { items: Vec<&'a Kind> },
}

impl<'a> KindIterMetas<'a> {
    fn push(&mut self, item: &'a Kind) {
        match self {
            KindIterMetas::Zero => {
                *self = KindIterMetas::One(item);
            }
            KindIterMetas::One(other_item) => {
                let items: Vec<&'a Kind> = vec![other_item, item];
                *self = KindIterMetas::Many { items };
            }
            KindIterMetas::Many { items } => {
                items.push(item);
            }
        }
    }

    fn pop(&mut self) -> Option<&'a Kind> {
        let (result, m_new_self): (Option<&'a Kind>, Option<KindIterMetas>) = match self {
            KindIterMetas::Zero => (None, None),
            KindIterMetas::One(item) => (Some(item), Some(KindIterMetas::Zero)),
            KindIterMetas::Many { items } => (items.pop(), None),
        };
        if let Some(new_self) = m_new_self {
            *self = new_self;
        }
        result
    }
}

impl<'a> Iterator for KindIterMetas<'a> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        fn step_kind(kind: &Kind) -> Step<Kind, usize> {
            match kind {
                Kind::Ref(kind) => match kind.as_ref() {
                    KindCompound::Arrow(a, b) => Step::Continue2(a, b),
                },
                Kind::Type => Step::Skip,
                Kind::Row => Step::Skip,
                Kind::Constraint => Step::Skip,
                Kind::Meta(n) => Step::Yield(*n),
            }
        }

        let mut res = None;
        loop {
            match self.pop() {
                None => {
                    break;
                }
                Some(kind) => match step_kind(kind) {
                    Step::Yield(n) => {
                        res = Some(n);
                        break;
                    }
                    Step::Skip => {
                        continue;
                    }
                    Step::Continue1(item) => {
                        self.push(item);
                        continue;
                    }
                    Step::Continue2(item1, item2) => {
                        self.push(item2);
                        self.push(item1);
                        continue;
                    }
                    Step::Continue3(item1, item2, item3) => {
                        self.push(item3);
                        self.push(item2);
                        self.push(item1);
                        continue;
                    }
                    Step::Continue(items) => {
                        for item in items.iter().rev() {
                            self.push(item);
                        }
                        continue;
                    }
                },
            }
        }
        res
    }
}
