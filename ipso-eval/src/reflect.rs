use std::{cmp::Ordering, rc::Rc};

use crate::{Interpreter, Object, Value};

pub trait ReflectRef {
    fn reflect_ref(&self, interpreter: &mut Interpreter) -> Value;
}

/** Ipso type: `(| Some : _, None : () |)`

```text
[[ reflect(None) ]] = None () : (| Some : _, None : () |)
[[ reflect(Some(a)) ]] = Some [[ reflect(a) ]] : (| Some : [[ type_of(a) ]], None : () |)
```
*/
impl<T: ReflectRef> ReflectRef for Option<T> {
    fn reflect_ref(&self, interpreter: &mut Interpreter) -> Value {
        let object = match self {
            Some(value) => Object::Variant(1, value.reflect_ref(interpreter)),
            None => Object::Variant(0, Value::Unit),
        };
        interpreter.alloc(object)
    }
}

impl ReflectRef for Value {
    fn reflect_ref(&self, _interpreter: &mut Interpreter) -> Value {
        self.clone()
    }
}

impl ReflectRef for &Value {
    fn reflect_ref(&self, _interpreter: &mut Interpreter) -> Value {
        (*self).clone()
    }
}

/// Ipso type: `Bool`
impl ReflectRef for bool {
    fn reflect_ref(&self, _interpreter: &mut Interpreter) -> Value {
        if *self {
            Value::True
        } else {
            Value::False
        }
    }
}

/// Ipso type: `String`
impl ReflectRef for Rc<str> {
    fn reflect_ref(&self, interpreter: &mut Interpreter) -> Value {
        interpreter.alloc(Object::String(self.clone()))
    }
}

/// Ipso type: `Bytes`
impl ReflectRef for Rc<[u8]> {
    fn reflect_ref(&self, interpreter: &mut Interpreter) -> Value {
        interpreter.alloc(Object::Bytes(self.clone()))
    }
}

/** Ipso type: `(| Less : (), Equal : (), Greater : () |)`

```text
[[ reflect(Less) ]] = Less () : (| Equal : (), Greater : (), Less : () |)
[[ reflect(Equal) ]] = Equal () : (| Equal : (), Greater : (), Less : () |)
[[ reflect(Greater) ]] = Greater () : (| Equal : (), Greater : (), Less : () |)
```
*/
impl ReflectRef for Ordering {
    fn reflect_ref(&self, interpreter: &mut Interpreter) -> Value {
        match self {
            std::cmp::Ordering::Equal => {
                // Equal () : (| Equal : (), Greater : (), Less : () |)
                interpreter.alloc(Object::Variant(0, Value::Unit))
            }
            std::cmp::Ordering::Greater => {
                // Greater () : (| Equal : (), Greater : (), Less : () |)
                interpreter.alloc(Object::Variant(1, Value::Unit))
            }
            std::cmp::Ordering::Less => {
                // Less () : (| Equal : (), Greater : (), Less : () |)
                interpreter.alloc(Object::Variant(2, Value::Unit))
            }
        }
    }
}

/// Ipso type: `Array _`
impl<T: ReflectRef> ReflectRef for [T] {
    fn reflect_ref(&self, interpreter: &mut Interpreter) -> Value {
        // Can we somehow avoid the extra allocation when `T = Value`?
        let values: Rc<[Value]> = self
            .iter()
            .map(|value| value.reflect_ref(interpreter))
            .collect::<Vec<_>>()
            .into();
        interpreter.alloc(Object::Array(values))
    }
}

/// Ipso type: `Array _`
impl<T: ReflectRef> ReflectRef for &[T] {
    fn reflect_ref(&self, interpreter: &mut Interpreter) -> Value {
        (*self).reflect_ref(interpreter)
    }
}

/// Ipso type: `Array _`
impl<T: ReflectRef> ReflectRef for Vec<T> {
    fn reflect_ref(&self, interpreter: &mut Interpreter) -> Value {
        self.as_slice().reflect_ref(interpreter)
    }
}

/// Ipso type: `Array _`
impl ReflectRef for Rc<[Value]> {
    fn reflect_ref(&self, interpreter: &mut Interpreter) -> Value {
        interpreter.alloc(Object::Array(self.clone()))
    }
}

/// Ipso type: `Int`
impl ReflectRef for i32 {
    fn reflect_ref(&self, _interpreter: &mut Interpreter) -> Value {
        Value::Int(*self)
    }
}

/// Ipso type: `String`
impl ReflectRef for str {
    fn reflect_ref(&self, interpreter: &mut Interpreter) -> Value {
        interpreter.alloc(Object::String(Rc::from(self)))
    }
}

/// Ipso type: `String`
impl ReflectRef for &str {
    fn reflect_ref(&self, interpreter: &mut Interpreter) -> Value {
        (*self).reflect_ref(interpreter)
    }
}

/// Ipso type: `Char`
impl ReflectRef for char {
    fn reflect_ref(&self, _interpreter: &mut Interpreter) -> Value {
        Value::Char(*self)
    }
}

/// Ipso type: `Char`
impl ReflectRef for &char {
    fn reflect_ref(&self, _interpreter: &mut Interpreter) -> Value {
        Value::Char(**self)
    }
}

/// Ipso type: `String`
impl ReflectRef for String {
    fn reflect_ref(&self, interpreter: &mut Interpreter) -> Value {
        interpreter.alloc(Object::String(Rc::from(self.as_str())))
    }
}

pub trait Reflect: ReflectRef + Sized {
    fn reflect(self, interpreter: &mut Interpreter) -> Value {
        self.reflect_ref(interpreter)
    }
}

/** Ipso type: `(| Some : _, None : () |)`

```text
[[ reflect(None) ]] = None () : (| Some : _, None : () |)
[[ reflect(Some(a)) ]] = Some [[ reflect(a) ]] : (| Some : [[ type_of(a) ]], None : () |)
```
*/
impl<T: Reflect> Reflect for Option<T> {
    fn reflect(self, interpreter: &mut Interpreter) -> Value {
        let object = match self {
            Some(value) => Object::Variant(1, value.reflect(interpreter)),
            None => Object::Variant(0, Value::Unit),
        };
        interpreter.alloc(object)
    }
}

impl Reflect for Value {
    fn reflect(self, _interpreter: &mut Interpreter) -> Value {
        self
    }
}

impl Reflect for &Value {
    fn reflect(self, _interpreter: &mut Interpreter) -> Value {
        self.clone()
    }
}

/// Ipso type: `Bool`
impl Reflect for bool {}

/// Ipso type: `Char`
impl Reflect for char {}

/// Ipso type: `Char`
impl Reflect for &char {
    fn reflect(self, _interpreter: &mut Interpreter) -> Value {
        Value::Char(*self)
    }
}

/// Ipso type: `String`
impl Reflect for Rc<str> {}

/// Ipso type: `Bytes`
impl Reflect for Rc<[u8]> {}

/** Ipso type: `(| Less : (), Equal : (), Greater : () |)`

```text
[[ reflect(Less) ]] = Less () : (| Equal : (), Greater : (), Less : () |)
[[ reflect(Equal) ]] = Equal () : (| Equal : (), Greater : (), Less : () |)
[[ reflect(Greater) ]] = Greater () : (| Equal : (), Greater : (), Less : () |)
```
*/
impl Reflect for Ordering {}

/// Ipso type: `Array _`
impl<T: ReflectRef> Reflect for &[T] {
    fn reflect(self, interpreter: &mut Interpreter) -> Value {
        self.reflect_ref(interpreter)
    }
}

/// Ipso type: `Array _`
impl<T: ReflectRef> Reflect for Vec<T> {
    fn reflect(self, interpreter: &mut Interpreter) -> Value {
        self.as_slice().reflect_ref(interpreter)
    }
}

/// Ipso type: `Array _`
impl Reflect for Rc<[Value]> {
    fn reflect(self, interpreter: &mut Interpreter) -> Value {
        interpreter.alloc(Object::Array(self))
    }
}

/// Ipso type: `Int`
impl Reflect for i32 {
    fn reflect(self, _interpreter: &mut Interpreter) -> Value {
        Value::Int(self)
    }
}

/// Ipso type: `String`
impl Reflect for &str {
    fn reflect(self, interpreter: &mut Interpreter) -> Value {
        self.reflect_ref(interpreter)
    }
}

/// Ipso type: `String`
impl Reflect for String {
    fn reflect(self, interpreter: &mut Interpreter) -> Value {
        interpreter.alloc(Object::String(Rc::from(self)))
    }
}
