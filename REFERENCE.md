# `ipso` language reference

## Contents

* [Datatypes](#datatypes)
    * [Booleans](#booleans)
    * [Integers](#integers)
    * [Characters](#characters)
    * [Strings](#strings)
    * [Functions](#functions)
    * [Arrays](#arrays)
    * [Records](#records)
    * [Variants](#variants)
* [Type classes](#type-classes)
    * [Equality](#equality)
    * [Comparison](#comparison)
    * [JSON](#json)

## Datatypes

### Booleans

```ipso-repl
> :type true
Int
```

```ipso-repl
> :type false
Int
```

```ipso-repl
> if true then "yes" else "no"
"yes"
```

```ipso-repl
> if false then "yes" else "no"
"no"
```

### Integers

```ipso-repl
> :type 0
Int
```

```ipso-repl
> :type 1
Int
```

```ipso-repl
> :type -1
Int
```

#### Builtins

```ipso
(+) : Int -> Int -> Int

(-) : Int -> Int -> Int

(*) : Int -> Int -> Int

(/) : Int -> Int -> Int
```

### Characters

The `Char` type represents a unicode code point.

```ipso-repl
> :type 'a'
Char
```

```ipso-repl
> :type '🤩'
Char
```

#### Builtins

```ipso
prev : Char -> Char

next : Char -> Char
```

### Strings

The `String` type is a UTF-8 encoded sequence of bytes.

```ipso-repl
> :type "hello"
String
```

```ipso-repl
> x = "hello"
x : String
> "${x} world"
"hello world"
```

#### Builtins

```ipso
foldr : (Char -> a -> a) -> String -> a -> a

foldl : (a -> Char -> a) -> String -> a -> a

map : (Char -> Char) -> String -> String

pack : Array Char -> String

unpack : String -> Array Char
```

### Functions

```ipso-repl
> :type \x -> x
forall t0. t0 -> t0
```

```ipso-repl
> :type \x y -> x + y
Int -> Int -> Int
```

```ipso-repl
> f x = x + 1
f : Int -> Int
> f 2
3
```

### Arrays

```ipso-repl
> :type [1, 2, 3]
Array Int
```

```ipso-repl
> :type [1, true, 3]
<repl>:1:5: error: expected 'Int', got 'Bool'
  |
1 | [1, true, 3]
  |     ^^^^
```

#### Builtins

```ipso
cons : a -> Array a -> Array a

uncons : Array a -> < None, Some : { first : a, rest : Array a } >

snoc : Array a -> a -> Array a

unsnoc : Array a -> < None, Some : { rest : Array a, last : a } >

(++) : Array a -> Array a -> Array a

foldr : (a -> b -> b) -> b -> Array a -> b

foldl : (b -> a -> b) -> b -> Array a -> b

map : (a -> b) -> Array a -> Array b
```

### Records

```ipso-repl
> :t { x = 1, y = true }
{ x : Int, y : Bool }
```

```ipso-repl
> x = { a = "hello", b = false }
x : { a : String, b : Bool }
> x.a
"hello"
> x.b
false
```

```ipso-repl
> rest = { more = false, words = ["a", "b"] }
rest : { more : Bool, words : Array String }
> :t { some = "some" | rest }
{ some : String, more : Bool, words : Array String }
```

```ipso-repl
> a = 1
a : Int
> b = true
b : Bool
> { a, b }
{ a = 1, b = True }
```

### Variants

```ipso-repl
> :t None
forall r0. < None | r0 >
```

```ipso-repl
> x = None
x : forall r0 . < None | r0 >
> case x of
.   None -> 1
.   _ -> 2
.
1
```

```ipso-repl
> x = None
x : forall r0 . < None | r0 >
> case x of
.   None -> 1
.
<repl>:1:1 error: missing patterns
  • _ -> ...
```

```ipso-repl
> x = None : < None >
x : < None >
> case x of
.   None -> 1
.
1
```

## Type classes

### Equality

```ipso
class Eq a where
  (==) : a -> a -> Bool
  
(!=) : a -> a -> Bool
```

```ipso
instance Eq Bool

instance Eq Char

instance Eq String

instance Eq Int

instance Eq a => Eq (Array a)

instance Fields Eq row => Eq (Record row)

instance Fields Eq row => Eq (Variant row)
```

### Comparison

```ipso
class Eq a => Ord a where
  compare : a -> a -> { lt | eq | gt }
  
(<=) : a -> a -> Bool

(<) : a -> a -> Bool

(>=) : a -> a -> Bool

(>) : a -> a -> Bool
```

```ipso
instance Ord Bool

instance Ord Char

instance Ord String

instance Ord Int

instance Ord a => Ord (Array a)
```

### JSON

```ipso
class ToJson a where
  encoder : a -> Json
  
 toJson : ToJson a => a -> String
```

```ipso
instance ToJson Bool
instance ToJson Int
instance ToJson Char
instance ToJson String
instance ToJson a => ToJson (Array a)
```

```
class FromJson a where
  decoder : Decoder a
  
fromJson : FromJson a => String -> < Err : JsonError, Ok : a >
```

```ipso
instance FromJson Bool
instance FromJson Int
instance FromJson Char
instance FromJson String
instance FromJson a => ToJson (Array a)
```
