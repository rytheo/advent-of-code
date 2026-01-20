use derive_more::{Deref, DerefMut};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;
use std::ops::{Index, IndexMut};

static ZERO: usize = 0;

#[derive(Clone, Debug, Default, Deref, DerefMut)]
pub struct Counter<T> {
    map: HashMap<T, usize>,
}

impl<T> Counter<T> {
    pub fn new() -> Self {
        Counter {
            map: HashMap::new(),
        }
    }
}

impl<T, Q: ?Sized> Index<&Q> for Counter<T>
where
    T: Eq + Hash + Borrow<Q>,
    Q: Eq + Hash,
{
    type Output = usize;

    fn index(&self, key: &Q) -> &Self::Output {
        self.map.get(key).unwrap_or(&ZERO)
    }
}

impl<T, Q: ?Sized> IndexMut<&Q> for Counter<T>
where
    T: Eq + Hash + Borrow<Q>,
    Q: Eq + Hash + ToOwned<Owned = T>,
{
    fn index_mut(&mut self, key: &Q) -> &mut Self::Output {
        self.map.entry(key.to_owned()).or_default()
    }
}

impl<T> FromIterator<T> for Counter<T>
where
    T: Eq + Hash + Clone,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut counter = Counter::new();
        for item in iter {
            counter[&item] += 1;
        }
        counter
    }
}

impl<T> IntoIterator for Counter<T> {
    type Item = (T, usize);
    type IntoIter = std::collections::hash_map::IntoIter<T, usize>;

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}
