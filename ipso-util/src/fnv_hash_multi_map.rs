use fnv::FnvHashMap;
use std::{collections::hash_map::Entry, hash::Hash, iter::FromIterator};

pub struct FnvHashMultimap<K, V>(FnvHashMap<K, Vec<V>>);

impl<K, V> FnvHashMultimap<K, V> {
    pub fn new() -> Self {
        FnvHashMultimap(FnvHashMap::default())
    }

    pub fn with_capacity(capacity: usize) -> Self {
        FnvHashMultimap(FnvHashMap::with_capacity_and_hasher(
            capacity,
            Default::default(),
        ))
    }
}

impl<K, V> Default for FnvHashMultimap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Eq + Hash, V> FnvHashMultimap<K, V> {
    pub fn contains<Key: ?Sized>(&mut self, item: &Key) -> bool
    where
        K: std::borrow::Borrow<Key>,
        Key: Eq + Hash,
    {
        self.0.contains_key(item)
    }

    pub fn get<Key: ?Sized>(&mut self, key: &Key) -> Option<&V>
    where
        K: std::borrow::Borrow<Key>,
        Key: Eq + Hash,
    {
        self.0.get(key).and_then(|values| values.last())
    }

    pub fn insert(&mut self, key: K, value: V) {
        match self.0.entry(key) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().push(value);
            }
            Entry::Vacant(entry) => {
                entry.insert(vec![value]);
            }
        }
    }

    pub fn remove<Key: ?Sized>(&mut self, key: &Key) -> Option<V>
    where
        K: std::borrow::Borrow<Key>,
        Key: Eq + Hash,
    {
        if let Some(values) = self.0.get_mut(key) {
            let value = match values.pop() {
                None => {
                    unreachable!()
                }
                Some(value) => value,
            };

            if values.is_empty() {
                self.0.remove(key);
            }

            Some(value)
        } else {
            None
        }
    }

    pub fn reverse_values(&mut self) {
        self.0.values_mut().for_each(|values| values.reverse())
    }
}

impl<K: Eq + Hash, V> FromIterator<(K, V)> for FnvHashMultimap<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut multiset = Self::new();
        for (key, value) in iter {
            multiset.insert(key, value);
        }
        multiset
    }
}
