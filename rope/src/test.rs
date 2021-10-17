#[cfg(test)]
use crate::Rope;

#[test]
fn rope_test_1() {
    assert_eq!(
        Rope::from_vec(&[0, 1, 2, 3, 4])
            .delete(1)
            .map(|xs| xs.iter().collect()),
        Ok(vec![&0, &2, &3, &4])
    )
}

#[test]
fn rope_test_2() {
    assert_eq!(
        Rope::from_vec(&[0, 1, 2, 3, 4])
            .delete(0)
            .map(|xs| xs.iter().collect()),
        Ok(vec![&1, &2, &3, &4])
    )
}

#[test]
fn rope_test_3() {
    assert_eq!(
        Rope::from_vec(&[0, 1, 2, 3, 4])
            .delete(4)
            .map(|xs| xs.iter().collect()),
        Ok(vec![&0, &1, &2, &3])
    )
}

#[test]
fn rope_test_4() {
    assert_eq!(
        Rope::from_vec(&[0, 1, 2, 3, 4])
            .delete(1)
            .unwrap()
            .delete(2)
            .map(|xs| xs.iter().collect()),
        Ok(vec![&0, &2, &4])
    )
}

#[test]
fn rope_test_5() {
    assert_eq!(
        Rope::from_vec(&[0, 1, 2, 3, 4])
            .delete(3)
            .unwrap()
            .delete(1)
            .map(|xs| xs.iter().collect()),
        Ok(vec![&0, &2, &4])
    )
}

#[test]
fn rope_test_6() {
    assert_eq!(
        Rope::from_vec(&[0, 1])
            .delete(0)
            .unwrap()
            .delete(0)
            .map(|xs| xs.iter().collect()),
        Ok(Vec::new())
    )
}

#[test]
fn rope_test_7() {
    assert_eq!(
        Rope::from_vec(&[("a", 0), ("b", 1), ("b", 2), ("c", 3)])
            .delete_first(|(x, _)| *x == "b")
            .map(|xs| xs.iter().collect()),
        Ok(vec![&("a", 0), &("b", 2), &("c", 3)])
    )
}

#[test]
fn rope_test_8() {
    assert_eq!(
        Rope::from_vec(&[("a", 0), ("b", 1), ("b", 2), ("c", 3)])
            .delete_first(|(x, _)| *x == "b")
            .unwrap()
            .delete_first(|(x, _)| *x == "b")
            .map(|xs| xs.iter().collect()),
        Ok(vec![&("a", 0), &("c", 3)])
    )
}

#[test]
fn rope_insert_at_1() {
    let initial = vec![0, 1, 2];
    let mut rope: Rope<usize> = Rope::from_vec(&initial);
    rope.insert_at(0, &3);
    rope.insert_at(0, &4);
    let expected: Vec<usize> = vec![4, 3, 0, 1, 2];
    let actual: Vec<usize> = rope.iter().copied().collect::<Vec<usize>>();
    assert_eq!(expected, actual)
}

#[test]
fn rope_insert_at_2() {
    let initial = vec![0, 1, 2];
    let mut rope: Rope<usize> = Rope::from_vec(&initial);
    rope.insert_at(1, &3);
    rope.insert_at(3, &4);
    let expected: Vec<usize> = vec![0, 3, 1, 4, 2];
    let actual: Vec<usize> = rope.iter().copied().collect::<Vec<usize>>();
    assert_eq!(expected, actual)
}

#[test]
fn rope_insert_at_3() {
    let initial = vec![0, 1, 2];
    let mut rope: Rope<usize> = Rope::from_vec(&initial);
    rope.insert_at(2, &4);
    rope.insert_at(1, &3);
    let expected: Vec<usize> = vec![0, 3, 1, 4, 2];
    let actual: Vec<usize> = rope.iter().copied().collect::<Vec<usize>>();
    assert_eq!(expected, actual)
}
