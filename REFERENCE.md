# `ipso` language reference

<!-- omit in toc -->
## Contents

* [Packages](#packages)
  * [Using Packages](#using-packages)
* [Modules](#modules)
  * [Comments](#comments)
  * [Declarations](#declarations)
  * [Type Aliases](#type-aliases)
  * [Imports](#imports)
    * [Basic Imports](#basic-imports)
    * [Renaming Imports](#renaming-imports)
    * [Selective Imports](#selective-imports)
    * [Wildcard Imports](#wildcard-imports)
  * [Execution](#execution)
* [Pattern Matching](#pattern-matching)
* [Let Bindings](#let-bindings)
* [Computation Expressions](#computation-expressions)
* [Command Literals](#command-literals)
  * [Interpolation](#interpolation)
* [Operators](#operators)
* [Datatypes](#datatypes)
  * [Booleans](#booleans)
  * [Integers](#integers)
  * [Characters](#characters)
    * [Builtins](#builtins)
  * [Strings](#strings)
    * [Builtins](#builtins-1)
  * [Functions](#functions)
  * [Arrays](#arrays)
    * [Builtins](#builtins-2)
  * [Byte Arrays](#byte-arrays)
  * [Records](#records)
  * [Variants](#variants)
    * [Construction](#construction)
    * [Extension](#extension)
  * [IO](#io)
    * [Builtins](#builtins-3)
  * [Commands](#commands)
    * [Builtins](#builtins-4)
* [Type Classes](#type-classes)
  * [Equality](#equality)
  * [Comparison](#comparison)
  * [JSON](#json)
* [Grammar](#grammar)
    
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

### Comments

```ipso
# this is a single line comment
```

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

## Pattern Matching

```ipso-repl
> x = None
x : forall r. (| None, r |)
> case x of
.   None -> 1
.   _ -> 2
.
1
```

```ipso-repl
> x = None
x : forall r. (| None, r |)
> case x of
.   None -> 1
.
(repl):1:1 error: missing patterns
  • _ -> ...
```

```ipso-repl
> x = None : (| None |)
x : (| None |)
> case x of
.   None -> 1
.
1
```

```ipso-repl
> case 'a' of
.   'b' -> "b"
.   'a' -> "a"
.   _ -> "something else"
.
"a"
```

```ipso-repl
> case 1 of
.   0 -> "0"
.   1 -> "1"
.   _ -> "something else"
.
"1"
```

```ipso-repl
> case "true" of
.   "false" -> 0
.   "true" -> 1
.   _ -> 2
.
1
```

## Let Bindings

```ipso-repl
> let x = 1 in
. let y = 2 in
. x + y
3
```

## Computation Expressions

```ipso-repl
> :t comp
.   x <- getLine
.   print x
.
IO ()
```

```ipso-repl
> :t comp
.   x <- getLine
.   return x
.
IO String
```

## Command Literals

```ipso-repl
> :kind Cmd
Type
```

```ipso-repl
> :t `ls -laR`
Cmd
```

```ipso-repl
> :t run
Cmd -> IO () 
```

```ipso-repl
> run ``
```

```ipso-repl
> run `echo "hello!"`
hello!
```

### Interpolation

```ipso-repl
> :type \x -> `echo $x`
ToArgs a => a -> Cmd
```

```ipso-repl
> let arg = "hi"
> `echo $arg`
`echo hi`
```

```ipso-repl
> let args = ["hello", "world"]
> `echo $arg`
`echo hello world`
```

## Operators

```ipso
== : Eq a => a -> a -> Bool

!= : Eq a => a -> a -> Bool

<= : Ord a => a -> a -> Bool

< : Ord a => a -> a -> Bool

>= : Ord a => a -> a -> Bool

> : Ord a => a -> a -> Bool

&& : Bool -> Bool -> Bool

|| : Bool -> Bool -> Bool

+ : Int -> Int -> Int

- : Int -> Int -> Int

* : Int -> Int -> Int

/ : Int -> Int -> Int

++ : Array a -> Array a -> Array a

<| : (a -> b) -> a -> b

|> : a -> (a -> b) -> b
```

## Datatypes

### Booleans

```ipso-repl
> :type true
Bool
```

```ipso-repl
> :type false
Bool
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
foldr : (Char -> a -> a) -> a -> String -> a

foldlString : (a -> Char -> a) -> a -> String -> a

map : (Char -> Char) -> String -> String

filterString : (Char -> Bool) -> String -> String

splitString : Char -> String -> Array String

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

uncons : Array a -> (| None, Some : { first : a, rest : Array a } |)

snocArray : Array a -> a -> Array a

unsnoc : Array a -> (| None, Some : { rest : Array a, last : a } |)

lengthArray : Array a -> Int

indexArray : Int -> Array a -> a

sliceArray : Int -> Int -> Array a -> Array a

foldr : (a -> b -> b) -> b -> Array a -> b

foldlArray : (b -> a -> b) -> b -> Array a -> b

map : (a -> b) -> Array a -> Array b

generateArray : Int -> (Int -> a) -> Array a

flatMap : (a -> Array b) -> Array a -> Array b
```

### Byte Arrays

```ipso-repl
> :kind Bytes
Type
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

#### Construction

```ipso-repl
> :t None
forall r. (| None, r |)
```

#### Extension

```ipso-repl
> :t \x -> (| A, ..x |)
(| r |) -> (| A : a, r |)
```

### IO

```ipso-repl
> :kind IO
Type -> Type
```

```ipso-repl
> comp
.   line <- getLine
.   print line
hello
hello
```

#### Builtins

```ipso
pure : a -> IO a

mapIO : (a -> b) -> IO a -> IO b

bindIO : IO a -> (a -> IO b) -> IO b
```

```ipso
println : String -> IO ()

print : String -> IO ()
```

```ipso
readln : IO String
```

### Commands

```ipso-repl
> :kind Cmd
Type
```

```ipso-repl
> :type `echo "hello, world!"
Cmd
```

#### Builtins

```ipso
run : Cmd -> IO ()

lines : Cmd -> IO (Array String)

showCmd : Cmd -> String

class ToArgs a where
  toArgs : a -> Array String

instance ToArgs String

instance ToArgs a => ToArgs (Array a) 
```

## Type Classes

### Equality

```ipso
class Eq a where
  eq : a -> a -> Bool
  
neq : Eq a => a -> a -> Bool
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
  compare : a -> a -> (| Less : (), Equal : (), Greater : () |)
  
lte : a -> a -> Bool

lt : a -> a -> Bool

gte : a -> a -> Bool

gt : a -> a -> Bool
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
  
fromJson : FromJson a => String -> (| Err : DecodeError, Ok : a |)
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
  '(|' type_variant_content '|)'
  
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
  let |
  comp |
  binop
  
lambda ::=
  '\' pattern '->' expr
  
case ::=
  'case' expr 'of' case_branch*
  
case_branch ::=
  ctor '->' expr
  
ifthenelse ::=
  'if' expr 'then' expr 'else' expr

let ::=
  'let' ident '=' expr 'in' expr

comp_line ::=
  expr
  'bind' ident '<-' expr
  'return' expr

comp ::=
  'comp' comp_line+
  
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
  cmd |
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

cmd_char ::=
  (any ascii character except '`', '$', '"', '\')
  '\' '`'
  '\' '$'
  '\' '"'
  '\' '\'

cmd_part ::=
  cmd_char+
  '$' ident
  '$' '{' expr '}'

cmd ::= 
  '`' cmd_part* '`'


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
