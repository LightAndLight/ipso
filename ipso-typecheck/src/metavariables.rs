//! Metavariables and solutions.

/// A metavariable.
pub type Meta = usize;

/// A metavariable solution.
#[derive(Clone)]
pub enum Solution<T> {
    Unsolved,
    Solved(T),
}

impl<T> Solution<T> {
    pub fn is_unsolved(&self) -> bool {
        match self {
            Solution::Unsolved => true,
            Solution::Solved(_) => false,
        }
    }
}

/// A mapping from metavariables to their solutions.
pub struct Solutions<T> {
    solutions: Vec<Solution<T>>,
}

/**
# Preconditions

* [`Meta`] arguments must be valid.

  `self.contains(meta)`

  Applies to: [`Solutions::get`], [`Solutions::set`]
*/
impl<T> Solutions<T> {
    pub fn new() -> Self {
        Solutions {
            solutions: Vec::new(),
        }
    }

    /**
    Check whether a metavariable is in the [`Solutions`]' domain.
    */
    pub fn contains(&self, meta: Meta) -> bool {
        meta < self.solutions.len()
    }

    /**
    Get a metavariable's solution.
    */
    pub fn get(&self, meta: Meta) -> &Solution<T> {
        self.solutions
            .get(meta)
            .unwrap_or_else(|| panic!("meta {:?} not found", meta))
    }

    /**
    Set a metavariable's solution.

    Each metavariable can only set once.

    # Preconditions

    * `self.get(meta).is_unsolved()`
    */
    pub fn set(&mut self, meta: Meta, value: &T)
    where
        T: Clone,
    {
        let solution = self
            .solutions
            .get_mut(meta)
            .unwrap_or_else(|| panic!("meta {:?} not found", meta));
        if solution.is_unsolved() {
            *solution = Solution::Solved(value.clone());
        } else {
            panic!("meta {:?} has already been set", meta);
        }
    }

    /**
    Generate a new, unsolved metavariable.
    */
    pub fn fresh_meta(&mut self) -> Meta {
        let m = self.solutions.len();
        self.solutions.push(Solution::Unsolved);
        m
    }
}

impl<T> Default for Solutions<T> {
    fn default() -> Self {
        Self::new()
    }
}
