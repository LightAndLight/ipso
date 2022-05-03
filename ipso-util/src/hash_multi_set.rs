use std::{
    collections::{hash_map::Entry, HashMap},
    hash::Hash,
    iter::FromIterator,
};

pub struct HashMultiset<A>(HashMap<A, usize>);

impl<A> HashMultiset<A> {
    pub fn new() -> Self {
        HashMultiset(HashMap::new())
    }
}

impl<A> Default for HashMultiset<A> {
    fn default() -> Self {
        Self::new()
    }
}

impl<A: Eq + Hash> HashMultiset<A> {
    pub fn contains<B: ?Sized>(&mut self, item: &B) -> bool
    where
        A: std::borrow::Borrow<B>,
        B: Eq + Hash,
    {
        self.0.contains_key(item)
    }

    pub fn insert(&mut self, item: A) {
        match self.0.entry(item) {
            Entry::Occupied(mut entry) => {
                *entry.get_mut() += 1;
            }
            Entry::Vacant(entry) => {
                entry.insert(1);
            }
        }
    }

    pub fn remove<B: ?Sized>(&mut self, item: &B)
    where
        A: std::borrow::Borrow<B>,
        B: Eq + Hash,
    {
        if let Some(count) = self.0.get_mut(item) {
            *count -= 1;
            if *count == 0 {
                self.0.remove(item);
            }
        }
    }

    pub fn insert_all(&mut self, iter: impl Iterator<Item = A>) {
        iter.for_each(|item| self.insert(item))
    }

    pub fn remove_all(&mut self, iter: impl Iterator<Item = A>) {
        iter.for_each(|item| self.remove(&item))
    }
}

impl<A: Eq + Hash> FromIterator<A> for HashMultiset<A> {
    fn from_iter<T: IntoIterator<Item = A>>(iter: T) -> Self {
        let mut multiset = Self::new();
        for item in iter {
            multiset.insert(item);
        }
        multiset
    }
}
