# `ipso` language reference

## Contents

- [`ipso` language reference](#ipso-language-reference)
  - [Contents](#contents)
  - [Packages](#packages)
    - [Using Packages](#using-packages)
  - [Modules](#modules)
    - [Declarations](#declarations)
    - [Type Aliases](#type-aliases)
    - [Imports](#imports)
      - [Basic Imports](#basic-imports)
      - [Renaming Imports](#renaming-imports)
      - [Selective Imports](#selective-imports)
      - [Wildcard Imports](#wildcard-imports)
    - [Execution](#execution)
  - [Datatypes](#datatypes)
    - [Booleans](#booleans)
    - [Integers](#integers)
      - [Builtins](#builtins)
    - [Characters](#characters)
      - [Builtins](#builtins-1)
    - [Strings](#strings)
      - [Builtins](#builtins-2)
    - [Functions](#functions)
    - [Arrays](#arrays)
      - [Builtins](#builtins-3)
    - [Byte Arrays](#byte-arrays)
      - [Builtins](#builtins-4)
    - [Records](#records)
    - [Variants](#variants)
    - [IO](#io)
      - [Builtins](#builtins-5)
  - [Type Classes](#type-classes)
    - [Equality](#equality)
    - [Comparison](#comparison)
    - [JSON](#json)
  - [Grammar](#grammar)
    
## Packages

A package is a directory that contains `.ipso` files.

```
my_package
├── a.ipso
├── b.ipso
└── b
    ├── c.ipso
    └── d.ipso
└── e.ipso
```

### Using Packages

Packages are made available to `ipso` via the `IPSO_PACKAGES` environment variable.

Example:

```bash
$ IPSO_PACKAGES=/path/to/a:/path/to/b ipso my_file.ipso
```
    
## Modules

### Declarations

```ipso
x = 1

y : String
y = "hello"
```

### Type Aliases

```ipso
type Person = { name : String, age : Int }
```

### Imports

#### Basic Imports

```ipso
import char
import string

loudly : String -> String
loudly = string.map char.to_upper
```

#### Renaming Imports

```ipso
import char as c
import string as s

loudly : String -> String
loudly = s.map c.to_upper
```

#### Selective Imports

```ipso
from char import to_upper
from string import map

loudly : String -> String
loudly = map to_upper
```

#### Wildcard Imports

```ipso
from char import *
from string import *

loudly : String -> String
loudly = map to_upper
```

### Execution

`ipso` will look for an IO action named `main` when called from the command line:

```
$ cat > example.ipso <<EOF
main : IO ()
main = print "hello"
EOF

$ ipso example.ipso
hello
```

This behaviour can be overridden with `-r`/`--run`:

```
$ cat > example.ipso <<EOF
sayHello : IO ()
sayHello = print "hello"
EOF

$ ipso example.ipso --run sayHello
hello
```

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
eq : String -> String -> Bool

foldr : (Char -> a -> a) -> String -> a -> a

foldl : (a -> Char -> a) -> String -> a -> a

map : (Char -> Char) -> String -> String

pack : Array Char -> String

unpack : String -> Array Char

toUtf8 : String -> Bytes
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

```ipso-repl
> f { x, y } = x + y
f : { x : Int, y : Int } -> Int
```

```ipso-repl
> :t \{ x, y } -> x + y
{ x : Int, y : Int } -> Int
```

```ipso-repl
> :t \{ x, ..rest } -> x + rest.y + rest.z
{ x : Int, y : Int, z : Int } -> Int
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

len : Array a -> Int

(++) : Array a -> Array a -> Array a

foldr : (a -> b -> b) -> b -> Array a -> b

foldl : (b -> a -> b) -> b -> Array a -> b

map : (a -> b) -> Array a -> Array b
```

### Byte Arrays

```ipso-repl
> :kind Bytes
Type
```

#### Builtins

```ipso
(++) : Bytes -> Bytes -> Bytes
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
> :t { some = "some", ..rest }
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
forall r. < None | r >
```

```ipso-repl
> x = None
x : forall r. < None | r >
> case x of
.   None -> 1
.   _ -> 2
.
1
```

```ipso-repl
> x = None
x : forall r. < None | r >
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

```ipso-repl
> :t \x -> < A | x >
< r > -> < A : a | r >
```

### IO

```ipso-repl
> :kind IO
Type -> Type
```

```ipso-repl
> do
.   line <- getLine
.   print line
hello
hello
```

```ipso-repl
> :kind Stdout
Type
```

```ipso-repl
> :info IOError
type IOError = < EBADF | EINTR | ENOSPC | EIO >
```

#### Builtins

```ipso
pureIO : a -> IO a

mapIO : (a -> b) -> IO a -> IO b

bindIO : IO a -> (a -> IO b) -> IO b
```

```ipso
writeStdout : Stdout -> Bytes -> IO < Err : IOError | Ok : () >
```

## Type Classes

### Equality

```ipso
class Eq a where
  (==) : a -> a -> Bool
  
(!=) : Eq a => a -> a -> Bool
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
  compare : a -> a -> < LT, EQ, GT >
  
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
instance Fields ToJson row => ToJson (Record row)
```

```
class FromJson a where
  decoder : Decoder a
  
fromJson : FromJson a => String -> < Err : DecodeError, Ok : a >
```

```ipso
instance FromJson Bool
instance FromJson Int
instance FromJson Char
instance FromJson String
instance FromJson a => ToJson (Array a)
instance Fields FromJson row => FromJson (Record row)
```

## Grammar

```
ident ::=
  ident_start ident_continue*
  
ident_start ::= (lowercase ASCII alphabetic characters)

ident_continue ::= 
  (ASCII alphanumeric characters) |
  '_'


ctor ::=
  ctor_start ctor_continue*
  
ctor_start ::= (uppercase ASCII alphabetic characters)

ctor_continue ::= 
  (ASCII alphanumeric characters) |
  '_'
  
  
type ::=
  type_app (type_arrow type)*
  
type_arrow ::=
  '->' |
  '=>'
  
type_app ::=
  type_atom+
  
type_atom ::=
  type_record |
  type_variant |
  ident |
  ctor |
  '(' type ')'
  
type_record ::=
  '{' type_record_content '}'
  
type_record_content ::=
  epsilon |
  type_signature (',' type_signature)* [',' ident] |
  ident
  
type_variant ::=
  '<' type_variant_content '>'
  
type_variant_content ::=
  epsilon |
  [type_variant_item ('|' type_variant_item)* ['|' ident]] |
  ident
  
type_variant_item ::=
  ctor [':' type]
  
  
pattern ::=
  ident |
  '{' record_pattern '}' |
  '_'
  
record_pattern ::=
  epsilon |
  ident (',' ident)* [',' '..' ident] |
  '..' ident |
 
 
expr ::=
  lambda |
  case |
  ifthenelse |
  binop
  
lambda ::=
  '\' pattern '->' expr
  
case ::=
  'case' expr 'of' case_branch*
  
case_branch ::=
  ctor '->' expr
  
ifthenelse ::=
  'if' expr 'then' expr 'else' expr
  
binop ::=
  app (operator app)*
  
operator ::=
  '=' (operator_symbol | '=')+
  operator_symbol+
  
operator_symbol ::=
  '+' |
  '-' |
  '*' |
  '/' |
  '<' |
  '>' |
  '^' |
  '%' |
  '|' |
  '&' |
  '!'
  
app ::=
  project+
  
project ::=
  atom ('.' ident)*
  
atom ::=
  bool |
  int |
  char |
  string |
  array |
  record |
  ident |
  ctor |
  '(' expr ')'
  
bool ::=
  'true' |
  'false'
  
int ::=
  (ASCII numeric character)+
  
char ::=
  "'" (any unicode code point except single quote) "'" |
  "'" '\' (single quote) "'"
  
string ::=
  '"' string_part* '"'
  
string_part ::=
  string_char+ |
  string_interpolate
  
string_char ::=
  (any unicode code point except double quote or dollar sign) |
  '\' '"' |
  '\' '$'
  
string_interpolate ::=
  '$' '{' expr '}'
  
array ::=
  '[' [expr (',' expr)*] ']'
  
record ::=
  '{' record_content '}'
  
record_content ::=
  epsilon |
  record_item (',' record_item)* [',' '..' atom] |
  '..' atom
  
record_item ::=
  ident '=' expr


decl ::=
  import |
  type_signature |
  definition |
  instance |
  class |
  type_alias


import ::=
  'import' module_name ['as' module_name] |
  'from' module_name 'import' from_imports
  
module_name ::=
  ident ('.' ident)*
  
from_imports ::=
  '*' |
  ident (',' ident)*


type_signature ::=
  ident ':' type


definition ::=
  ident [pattern] '=' expr
  

instance ::=
  'instance' type 'where' instance_member*
  
instance_member ::=
  [type_signature] definition
  

class ::=
  'class' type 'where' class_member*
  
class_member ::=
  type_signature
  
  
type_alias ::=
  'type' type '=' type
```
