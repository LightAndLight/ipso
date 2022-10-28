use ipso_util::iter::Stack;
use std::rc::Rc;

/**
A compound kind.

This type is part of an implementation strategy to reduce the size of [`Kind`].

[`Kind`] stores an `Rc<KindCompound>`, which costs 8 bytes. Every kind larger
than 8 bytes is added here, so that the size of [`Kind`] remains unchanged.
*/
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum KindCompound {
    Arrow(Kind, Kind),
}

impl KindCompound {
    pub fn mk_arrow(a: Kind, b: Kind) -> Self {
        KindCompound::Arrow(a, b)
    }
}

/**
# Size

Kinds are stored in every kind-checked type, so it's important that kinds
take up little space. This test ensured that no change increases the size of
`Kind`:

```
use ipso_syntax::kind::Kind;

assert_eq!(std::mem::size_of::<Kind>(), 16)
```

The situation could be improved by storing `&Kind` in checked types, which
costs 8 bytes instead of the current 16. It wouldn't be worth it if it requires
a 'viral' lifetime parameter everywhere types are used.

Kinds larger than 8 bytes should be added to [`KindCompound`].
*/
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Kind {
    Type,
    Row,
    Constraint,
    Meta(usize),
    Ref(Rc<KindCompound>),
}

impl Kind {
    pub fn iter_metas(&self) -> impl Iterator<Item = usize> + '_ {
        let mut stack = Stack::one(self);
        std::iter::from_fn(move || loop {
            match stack.pop() {
                None => {
                    return None;
                }
                Some(current) => match current {
                    Kind::Type | Kind::Row | Kind::Constraint => {
                        continue;
                    }
                    Kind::Ref(kind) => match kind.as_ref() {
                        KindCompound::Arrow(a, b) => {
                            stack.push(b);
                            stack.push(a);
                            continue;
                        }
                    },
                    Kind::Meta(n) => {
                        return Some(*n);
                    }
                },
            }
        })
    }

    pub fn mk_arrow(a: &Kind, b: &Kind) -> Self {
        Kind::Ref(Rc::new(KindCompound::Arrow(a.clone(), b.clone())))
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
