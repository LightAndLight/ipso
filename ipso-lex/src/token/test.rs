#[cfg(test)]
use crate::token::Name;
#[cfg(test)]
use quickcheck_macros::quickcheck;

#[cfg(test)]
#[quickcheck]
fn prop_from_int_to_int(n: Name) -> bool {
    match n {
        Name::Indent(_, _) => true,
        _ => Name::from_int(n.to_int()) == Some(n),
    }
}

#[cfg(test)]
#[quickcheck]
fn prop_to_int_from_int(n: usize) -> bool {
    match Name::from_int(n) {
        None => true,
        Some(name) => name.to_int() == n,
    }
}
