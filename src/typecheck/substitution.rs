use std::collections::HashMap;

use crate::syntax::Type;

use super::{TypeError, Typechecker, UnifyTypeContext};

mod test;

pub struct Substitution(HashMap<usize, Type<usize>>);

impl Substitution {
    pub fn new() -> Self {
        Substitution(HashMap::new())
    }

    pub fn into_hashmap(self) -> HashMap<usize, Type<usize>> {
        self.0
    }

    pub fn subst_left(
        &mut self,
        tc: &mut Typechecker,
        context: &UnifyTypeContext<usize>,
        expected: usize,
        actual: Type<usize>,
    ) -> Result<(), TypeError> {
        tc.occurs_type(expected, &actual)?;
        let m_expected_ty = tc.type_solutions[expected].1.clone().map(|x| x.clone());
        match m_expected_ty {
            None => {
                let m_expected_ty = self.0.get(&expected).map(|x| x.clone());
                match m_expected_ty {
                    None => {
                        for (_, current_ty) in self.0.iter_mut() {
                            *current_ty = (*current_ty).subst_metas(&mut |current_var| {
                                if current_var == expected {
                                    actual.clone()
                                } else {
                                    Type::Meta(current_var)
                                }
                            });
                        }
                        self.0.insert(expected, actual);
                        Ok(())
                    }
                    Some(expected_ty) => tc.unify_type_subst(self, context, expected_ty, actual),
                }?;
                Ok(())
            }
            Some(expected_ty) => tc.unify_type_subst(self, context, expected_ty, actual),
        }
    }

    pub fn subst_right(
        &mut self,
        tc: &mut Typechecker,
        context: &UnifyTypeContext<usize>,
        expected: Type<usize>,
        actual: usize,
    ) -> Result<(), TypeError> {
        tc.occurs_type(actual, &expected)?;
        let m_actual_ty = tc.type_solutions[actual].1.clone().map(|x| x.clone());
        match m_actual_ty {
            None => {
                let m_actual_ty = self.0.get(&actual).map(|x| x.clone());
                match m_actual_ty {
                    None => {
                        for (_, current_ty) in self.0.iter_mut() {
                            *current_ty = (*current_ty).subst_metas(&mut |current_var| {
                                if current_var == actual {
                                    expected.clone()
                                } else {
                                    Type::Meta(current_var)
                                }
                            });
                        }
                        self.0.insert(actual, expected);
                        Ok(())
                    }
                    Some(actual_ty) => tc.unify_type_subst(self, context, expected, actual_ty),
                }?;
                Ok(())
            }
            Some(actual_ty) => tc.unify_type_subst(self, context, expected, actual_ty),
        }
    }
}
