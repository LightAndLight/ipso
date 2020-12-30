mod test;

#[derive(PartialEq, Eq, Debug)]
pub enum Rope<'a, A> {
    Empty,
    Slice(&'a [A]),
    Item(&'a A),
    Branch(usize, Box<Rope<'a, A>>, Box<Rope<'a, A>>),
}

impl<'a, A> Rope<'a, A> {
    pub fn from_vec(v: &'a Vec<A>) -> Rope<'a, A> {
        Rope::Slice(v)
    }

    pub fn size(&self) -> usize {
        match self {
            Rope::Empty => 0,
            Rope::Item(_) => 1,
            Rope::Slice(slice) => slice.len(),
            Rope::Branch(size, _, _) => *size,
        }
    }

    pub fn insert_at(&mut self, ix: usize, val: &'a A) {
        match self {
            Rope::Empty => {
                *self = Rope::Item(val);
            }
            Rope::Item(a) => {
                if ix == 0 {
                    *self = Rope::Branch(2, Box::new(Rope::Item(val)), Box::new(Rope::Item(a)));
                } else {
                    *self = Rope::Branch(2, Box::new(Rope::Item(a)), Box::new(Rope::Item(val)));
                }
            }
            Rope::Slice(slice) => {
                let slice_len = slice.len();
                if ix < slice_len {
                    let (prefix, suffix) = slice.split_at(ix);
                    let prefix_len = prefix.len();
                    let suffix_len = suffix.len();
                    if prefix_len == 0 {
                        *self = Rope::Branch(
                            suffix_len + 1,
                            Box::new(Rope::Item(val)),
                            Box::new(Rope::Slice(suffix)),
                        );
                    } else if suffix_len == 0 {
                        *self = Rope::Branch(
                            prefix_len + 1,
                            Box::new(Rope::Slice(prefix)),
                            Box::new(Rope::Item(val)),
                        );
                    } else {
                        if prefix_len <= suffix_len {
                            *self = Rope::Branch(
                                prefix_len + suffix_len + 1,
                                Box::new(Rope::Branch(
                                    prefix_len + 1,
                                    Box::new(Rope::Slice(prefix)),
                                    Box::new(Rope::Item(val)),
                                )),
                                Box::new(Rope::Slice(suffix)),
                            );
                        } else {
                            *self = Rope::Branch(
                                prefix_len + suffix_len + 1,
                                Box::new(Rope::Slice(prefix)),
                                Box::new(Rope::Branch(
                                    suffix_len + 1,
                                    Box::new(Rope::Item(val)),
                                    Box::new(Rope::Slice(suffix)),
                                )),
                            )
                        }
                    }
                } else {
                    *self = Rope::Branch(
                        slice_len + 1,
                        Box::new(Rope::Slice(slice)),
                        Box::new(Rope::Item(val)),
                    );
                }
            }
            Rope::Branch(size, left, right) => {
                let left_size = left.size();
                if ix < left_size {
                    left.insert_at(ix, val);
                } else {
                    right.insert_at(ix - left_size, val);
                }
                *size += 1;
            }
        }
    }

    pub fn delete_first<P: Fn(&A) -> bool>(
        self,
        predicate: &P,
    ) -> Result<Rope<'a, A>, Rope<'a, A>> {
        match self {
            Rope::Empty => Err(Rope::Empty),
            Rope::Item(a) => {
                if predicate(a) {
                    Ok(Rope::Empty)
                } else {
                    Err(Rope::Item(a))
                }
            }
            Rope::Slice(slice) => match slice.iter().position(predicate) {
                Some(ix) => {
                    let (prefix, suffix) = slice.split_at(ix);
                    let suffix = &suffix[1..];
                    let prefix_len = prefix.len();
                    let suffix_len = suffix.len();
                    if prefix_len == 0 {
                        Ok(Rope::Slice(suffix))
                    } else if suffix_len == 0 {
                        Ok(Rope::Slice(prefix))
                    } else {
                        Ok(Rope::Branch(
                            prefix_len + suffix_len,
                            Box::new(Rope::Slice(prefix)),
                            Box::new(Rope::Slice(suffix)),
                        ))
                    }
                }
                None => Err(self),
            },
            Rope::Branch(size, mut left, mut right) => match left.delete_first(predicate) {
                Err(new_left) => {
                    *left = new_left;
                    match right.delete_first(predicate) {
                        Err(new_right) => {
                            *right = new_right;
                            Err(Rope::Branch(size, left, right))
                        }
                        Ok(new_right) => {
                            *right = new_right;
                            let left_size = left.size();
                            let right_size = right.size();
                            if right.size() == 0 {
                                Ok(*left)
                            } else {
                                Ok(Rope::Branch(left_size + right_size, left, right))
                            }
                        }
                    }
                }
                Ok(new_left) => {
                    *left = new_left;
                    let left_size = left.size();
                    let right_size = right.size();
                    if left.size() == 0 {
                        Ok(*right)
                    } else {
                        Ok(Rope::Branch(left_size + right_size, left, right))
                    }
                }
            },
        }
    }

    pub fn delete(self, ix: usize) -> Result<Rope<'a, A>, Rope<'a, A>> {
        match self {
            Rope::Empty => Err(Rope::Empty),
            Rope::Item(a) => {
                if ix == 0 {
                    Ok(Rope::Empty)
                } else {
                    Err(Rope::Item(a))
                }
            }
            Rope::Slice(slice) => {
                if ix < slice.len() {
                    let (prefix, suffix) = slice.split_at(ix);
                    let suffix = &suffix[1..];
                    let prefix_len = prefix.len();
                    let suffix_len = suffix.len();
                    if prefix_len == 0 {
                        if suffix_len == 0 {
                            Ok(Rope::Empty)
                        } else {
                            Ok(Rope::Slice(suffix))
                        }
                    } else if suffix_len == 0 {
                        Ok(Rope::Slice(prefix))
                    } else {
                        Ok(Rope::Branch(
                            prefix_len + suffix_len,
                            Box::new(Rope::Slice(prefix)),
                            Box::new(Rope::Slice(suffix)),
                        ))
                    }
                } else {
                    Err(Rope::Slice(slice))
                }
            }
            Rope::Branch(size, mut left, mut right) => {
                let left_size = left.size();
                if ix >= left_size {
                    match (*right).delete(ix - left_size) {
                        Err(new) => {
                            *right = new;
                            Err(Rope::Branch(size, left, right))
                        }
                        Ok(new) => {
                            if new.size() == 0 {
                                Ok(*left)
                            } else {
                                *right = new;
                                Ok(Rope::Branch(size - 1, left, right))
                            }
                        }
                    }
                } else {
                    match (*left).delete(ix) {
                        Err(new) => {
                            *left = new;
                            Err(Rope::Branch(size, left, right))
                        }
                        Ok(new) => {
                            if new.size() == 0 {
                                Ok(*right)
                            } else {
                                *left = new;
                                Ok(Rope::Branch(size - 1, left, right))
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn iter<'b>(&'b self) -> RopeIter<'b, 'a, A> {
        RopeIter {
            next: vec![self],
            current: [].iter(),
        }
    }
}

pub struct RopeIter<'b, 'a, A> {
    next: Vec<&'b Rope<'a, A>>,
    current: std::slice::Iter<'a, A>,
}

impl<'b, 'a, A> Iterator for RopeIter<'b, 'a, A> {
    type Item = &'a A;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current.next() {
            Some(val) => Some(val),
            None => match self.next.pop() {
                None => None,
                Some(next) => {
                    let mut next = next;
                    loop {
                        match next {
                            Rope::Empty => match self.next.pop() {
                                None => return None,
                                Some(val) => {
                                    next = val;
                                }
                            },
                            Rope::Item(a) => return Some(a),
                            Rope::Branch(_, a, b) => {
                                next = &*a;
                                self.next.push(&*b);
                            }
                            Rope::Slice(slice) => {
                                let mut slice_iter = slice.iter();
                                let next = slice_iter.next();
                                self.current = slice_iter;
                                return next;
                            }
                        }
                    }
                }
            },
        }
    }
}
