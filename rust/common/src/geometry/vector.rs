use std::ops::Neg;
use derive_more::{AsRef, AsMut, Index, IndexMut, IntoIterator};

type Int = i32;
type UInt = u32;

/// An N-dimensional vector.
#[derive(AsRef, AsMut, Clone, Copy, Hash, Index, IndexMut, IntoIterator, PartialEq, Eq, Ord, PartialOrd)]
pub struct Vector<const N: usize> {
    arr: [Int; N]
}

/// Returns a `Vector` containing the given values.
#[macro_export]
macro_rules! vecN {
    ($($num:expr),*) => {
        ::aoc::geometry::Vector::new([$($num),*])
    }
}

impl<const N: usize> Vector<N> {

    /// Returns a new `Vector` initialized with the given array.
    pub const fn new(arr: [Int; N]) -> Self {
        Self { arr }
    }

    /// Returns a `Vector` of zeros.
    pub const fn zero() -> Self {
        Self { arr: [0; N] }
    }

    /// Returns the number of dimensions of `self`.
    pub const fn len(&self) -> usize {
        self.arr.len()
    }

    /// Returns the Manhattan distance between `self` and `other`.
    pub fn dist_manhattan(self, other: Self) -> UInt {
        (self - other).abs().iter().map(|&x| x as UInt).sum()
    }

    /// Returns an iterator over the values in `self`.
    pub fn iter(&self) -> std::slice::Iter<Int> {
        self.arr.iter()
    }

    /// Returns an iterator that allows modifying the values in `self`.
    pub fn iter_mut(&mut self) -> std::slice::IterMut<Int> {
        self.arr.iter_mut()
    }

    /// Returns a `Vector` of the absolute values in `self`.
    pub fn abs(mut self) -> Self {
        for val in self.iter_mut() {
            *val = val.abs();
        }
        self
    }

    /// Returns an iterator over all points adjacent to `self`.
    pub fn adjacents(&self) -> Adjacents<N> {
        Adjacents { base: *self, dim: 0, delta: -1 }
    }

    /// Returns an iterator over all points up to one unit away in each dimension from `self`.
    pub fn neighbors(&self) -> Neighbors<N> {
        Neighbors { base: *self, delta: Self::zero() }
    }
}


/// Adjacent points iterator.
///
/// This struct is created by the `adjacents` method on `Vector`.
pub struct Adjacents<const N: usize> {
    base: Vector<N>,
    dim: usize,
    delta: Int,
}

impl<const N: usize> Iterator for Adjacents<N> {
    type Item = Vector<N>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.dim == N {
            return None;
        }
        let mut point = self.base;
        point[self.dim] += self.delta;
        self.delta = -self.delta;
        if self.delta == -1 {
            self.dim += 1;
        }
        Some(point)
    }
}

/// Neighboring points iterator.
///
/// This struct is created by the `neighbors` method on `Vector`.
pub struct Neighbors<const N: usize> {
    base: Vector<N>,
    delta: Vector<N>,
}

impl<const N: usize> Iterator for Neighbors<N> {
    type Item = Vector<N>;

    fn next(&mut self) -> Option<Self::Item> {
        for dim in 0..N {
            self.delta[dim] = match self.delta[dim] {
                0 => -1,
                -1 => 1,
                1 => 0,
                _ => unreachable!(),
            };
            if self.delta[dim] != 0 {
                return Some(self.base + self.delta);
            }
        }
        None
    }
}

impl<const N: usize> Neg for Vector<N> {
    type Output = Self;

    fn neg(mut self) -> Self {
        for val in self.iter_mut() {
            *val = val.neg();
        }
        self
    }
}

impl<const N: usize> std::fmt::Debug for Vector<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.arr)
    }
}

/// Implements binary operators on `Vector`.
macro_rules! impl_binary_ops {
    ($(($trait_:ident, $op_:ident)),*) => {
        $(
            impl<const N: usize> ::std::ops::$trait_ for Vector<N> {
                type Output = Self;

                fn $op_(mut self, other: Self) -> Self {
                    for i in 0..N {
                        self[i] = self[i].$op_(other[i])
                    }
                    self
                }
            }
        )*
    };
}

impl_binary_ops!((Add, add), (Sub, sub), (Mul, mul), (Div, div));

/// Implements assignment operators on `Vector`.
macro_rules! impl_assign_ops {
    ($(($trait_:ident, $op_:ident)),*) => {
        $(
            impl<const N: usize> std::ops::$trait_ for Vector<N> {
                fn $op_(&mut self, other: Self) {
                    for i in 0..N {
                        self[i].$op_(other[i])
                    }
                }
            }
        )*
    };
}

impl_assign_ops!((AddAssign, add_assign), (SubAssign, sub_assign), (MulAssign, mul_assign), (DivAssign, div_assign));
