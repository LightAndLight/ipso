---
title: Language Reference
---

<div style="display: flex; flex-direction: row">
<div id="contents">
<!-- omit in toc -->
## Contents

* [Execution](#execution)
* [Comments](#comments)
* [Indentation](#indentation)
  * [Line Joining](#line-joining)
  * [Blocks](#blocks)
  * [Examples](#examples)
* [Declarations](#declarations)
  * [Definitions](#definitions)
  * [Imports](#imports)
    * [Basic Imports](#basic-imports)
    * [Renaming Imports](#renaming-imports)
    * [Selective Imports](#selective-imports)
    * [Wildcard Imports](#wildcard-imports)
* [Pattern Matching](#pattern-matching)
  * [Records](#records)
  * [Variants](#variants)
  * [Literals](#literals)
* [Let Bindings](#let-bindings)
* [Computation Expressions](#computation-expressions)
* [Command Literals](#command-literals)
  * [Interpolation](#interpolation)
* [Operators](#operators)
* [Datatypes](#datatypes)
  * [Unit](#unit)
  * [Booleans](#booleans)
    * [Builtins](#builtins)
  * [Integers](#integers)
    * [Builtins](#builtins-1)
  * [Characters](#characters)
    * [Builtins](#builtins-2)
  * [Strings](#strings)
    * [Interpolation](#interpolation-1)
    * [Builtins](#builtins-3)
  * [Functions](#functions)
  * [Arrays](#arrays)
    * [Builtins](#builtins-4)
  * [Byte Arrays](#byte-arrays)
  * [Records](#records-1)
    * [Construction](#construction)
    * [Projection](#projection)
    * [Extension](#extension)
  * [Variants](#variants-1)
    * [Construction](#construction-1)
    * [Extension](#extension-1)
  * [IO](#io)
    * [Builtins](#builtins-5)
  * [Commands](#commands)
    * [Builtins](#builtins-6)
* [Type Classes](#type-classes)
  * [Equality](#equality)
  * [Comparison](#comparison)
  * [Debugging](#debugging)
* [Standard Library](#standard-library)
  * [`env`](#env)
  * [`exit`](#exit)
  * [`file`](#file)
  * [`path`](#path)
* [Grammar](#grammar)
</div>

<div style="margin-left: 3em; min-width: 0;">
<!-- omit in toc -->
# `ipso` language reference

## Execution

`ipso` will look for an [IO](#io) action named `main` when called from the command line:

```
$ cat > example.ipso <<EOF
main : IO ()
main = print "hello"
EOF

$ ipso example.ipso
hello
```

This behaviour can be overridden with `--run`:

```
$ cat > example.ipso <<EOF
sayHello : IO ()
sayHello = print "hello"
EOF

$ ipso example.ipso --run sayHello
hello
```

## Comments

```ipso
# this is a single line comment
```

## Indentation

`ipso` uses spaces (U+0020) for indentation.

### Line Joining

An indented line is considered a continuation of the preceding line.

### Blocks

The first line of a block must be indented further than the block's opening keyword.
Subsequent lines of a block must have indentation equal to its first line.

Keywords that open blocks:

* [`case ... of`](#pattern-matching)
* [`comp`](#computation-expressions)

### Examples

Correct:

```ipso
main : IO ()
main = comp
         println "hello"
         println "world"
```

```ipso
main : IO ()
main =
  comp
    println "hello"
    println "world"
```

```ipso
main : IO ()
main =
  comp
    println "hello"
    println
      "world"
```

```ipso
main : IO ()
main = case x of
         A a -> 0
         B b -> 1
```

```ipso
main : IO ()
main =
  case x of
    A a -> 0
    B b -> 1
```

```ipso
case x of
  A a ->
    0
  B b ->
    1
```

Incorrect:

```
main : IO ()
main = comp
  println "hello"
  println "world"
```

```
main : IO ()
main =
  comp
      println "hello"
    println "world"
```

```ipso
case x of
  A a -> 0
    B b -> 1
```

## Declarations

### Definitions

```ipso
x : Int
x = 1

y : String
y = "hello"
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

## Pattern Matching

### Records

```ipsorepl
> x = { a = "hi", b = true, c = 1 }
x : { a : String, b : Bool, c : Int }
> case x of
.   { a, b, c } -> b
.
true
> case x of
.   { b, ..rest } -> rest
.
{ a = "hi", c = 1 }
```

### Variants

```ipsorepl
> x = None ()
x : forall r. (| None : (), r |)
> case x of
.   None () -> 1
.   _ -> 2
.
1
```

```ipsorepl
> x = None () : (| None : () |)
x : (| None : () |)
> case x of
.   None () -> 1
.
1
```

```ipsorepl
> x = Ok (Ok 99)
x : forall r1 r2. (| Ok : (| Ok : Int, r1 |), r2 |)
> case x of
.   Ok (Ok a) -> a
.   _ -> 0
.
99
```

Incomplete pattern matches are not (yet) reported by the type checker:

```ipsorepl
> x = None ()
x : forall r. (| None : (), r |)
> case x of
.   None () -> 1
.
1
```

### Literals

```ipsorepl
> case 'a' of
.   'b' -> "b"
.   'a' -> "a"
.   _ -> "something else"
.
"a"
```

```ipsorepl
> case 1 of
.   0 -> "0"
.   1 -> "1"
.   _ -> "something else"
.
"1"
```

```ipsorepl
> case "true" of
.   "false" -> 0
.   "true" -> 1
.   _ -> 2
.
1
```

## Let Bindings

```ipsorepl
> let x = 1 in
. let y = 2 in
. x + y
3
```

## Computation Expressions

See also: [IO](#io)

```ipsorepl
> :t comp
.   bind x <- readln
.   let twoXs = "$x $x"
.   print twoXs
.
IO ()
```

```ipsorepl
> :t comp
.   bind x <- readln
.   return x
.
IO String
```

## Command Literals

See also: [Commands](#commands)

```ipsorepl
> :t `ls -laR`
Cmd
```

```ipsorepl
> cmd.run `echo "hello!"`
hello!
```

```ipsorepl
> cmd.run ``
```

### Interpolation

```ipsorepl
> let arg = "hi"
> `echo $arg`
`echo hi`
```

```ipsorepl
> let args = ["hello", "world"]
> `echo $..args`
`echo hello world`
```

```ipsorepl
> `echo ${ string.join " " ["hello", "world"] }`
`echo "hello world"`
```

```ipsorepl
> `echo $..{ ["a a", "b b"] ++ ["c c", "d d"] }`
`echo "a a" "b b" "c c" "d d"`
```

```ipsorepl
> :type \a b c d -> `echo $a $..b $c $..d`
String -> Array String -> String -> Array String -> Cmd
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

### Unit

```ipsorepl
> :type ()
()
```

### Booleans

```ipsorepl
> :type true
Bool
```

```ipsorepl
> :type false
Bool
```

```ipsorepl
> if true then "yes" else "no"
"yes"
```

```ipsorepl
> if false then "yes" else "no"
"no"
```

#### Builtins

```
not : Bool -> Bool
```

### Integers

```ipsorepl
> :type 0
Int
```

```ipsorepl
> :type 1
Int
```

```ipsorepl
> :type -1
Int
```

#### Builtins

```ipso
module int where

  eq : Int -> Int -> Bool

  toString : Int -> String

  mod : Int -> Int -> Int
```

### Characters

The `Char` type represents a unicode code point.

```ipsorepl
> :type 'a'
Char
```

```ipsorepl
> :type '🤩'
Char
```

#### Builtins

```ipso
module char where

  eq : Char -> Char -> Bool

  toString : Char -> String
```

### Strings

The `String` type is a UTF-8 encoded sequence of bytes.

```ipsorepl
> :type "hello"
String
```

#### Interpolation

```ipsorepl
> x = "hello"
x : String
> "$x world"
"hello world"
```

```ipsorepl
> x = ["a", "b", "c"]
x : Array String
> "joined: ${string.join ", " x}"
"joined: a, b, c"
```

#### Builtins

```ipso
module string where

  toUtf8 : String -> Bytes

  eq : String -> String -> Bool

  filter : (Char -> Bool) -> String -> String

  split : String -> String -> Array String
  
  splitc : Char -> String -> Array String

  join : String -> Array String -> String

  # `parts delimiter value` returns the sections of `value` that don't contain
  # the string `delimiter`.
  #
  # # Examples
  #
  # * `parts "::" "" == []`
  # * `parts "::" "a" == ["a"]`
  # * `parts "::" "a::b" == ["a", "b"]`
  # * `parts "::" "a::b::" == ["a", "b"]`
  # * `parts "!=" "a != b" == ["a ", " b"]`
  parts : String -> String -> Array String
  
  # `partsc delimiter value` returns the sections of `value` that don't contain
  # the character `delimiter`.
  #
  # # Examples
  #
  # * `partsc ' ' "" == []`
  # * `partsc ' ' "hello" == ["hello"]`
  # * `partsc ' ' "hello world" == ["hello", "world"]`
  # * `partsc ' ' "   hello    world   " == ["hello", "world"]`
  partsc : Char -> String -> Array String

  # Remove characters from both ends of a string, according to a predicate.
  trimp : (Char -> Bool) -> String -> String
  
  # Remove a specific character from both ends of a string.
  #
  # `trimc c1 = trimp (\c2 -> c2 == c1)`
  trimc : Char -> String -> String

  # Remove [whitespace](https://en.wikipedia.org/wiki/Unicode_character_property#Whitespace) from both ends of a string.
  trim : String -> String

  foldl : (a -> Char -> a) -> a -> String -> a
```

### Functions

```ipsorepl
> :type \x -> x
forall t0. t0 -> t0
```

```ipsorepl
> :type \x y -> x + y
Int -> Int -> Int
```

```ipsorepl
> f x = x + 1
f : Int -> Int
> f 2
3
```

```ipsorepl
> f { x, y } = x + y
f : { x : Int, y : Int } -> Int
```

```ipsorepl
> :t \{ x, y } -> x + y
{ x : Int, y : Int } -> Int
```

```ipsorepl
> :t \{ x, ..rest } -> x + rest.y + rest.z
{ x : Int, y : Int, z : Int } -> Int
```

### Arrays

```ipsorepl
> :type [1, 2, 3]
Array Int
```

```ipsorepl
> :type [1, true, 3]
(repl):1:5: error: expected 'Int', got 'Bool'
  |
1 | [1, true, 3]
  |     ^^^^
```

#### Builtins

```ipso
module array where

  eq : (a -> a -> Bool) -> Array a -> Array a -> Bool

  foldl : (b -> a -> b) -> b -> Array a -> b

  generate : Int -> (Int -> a) -> Array a

  length : Array a -> Int

  index : Int -> Array a -> a

  slice : Int -> Int -> Array a -> Array a

  snoc : Array a -> a -> Array a

  map : (a -> b) -> Array a -> Array b
  
  flatMap : (a -> Array b) -> Array a -> Array b

  unfoldr :
    s -> 
    (s -> (| Step : { value : a, next : s }, Skip : { next : s }, Done : () |)) -> 
    Array a

  sum : Array Int -> Int
  
  any : (a -> Bool) -> Array a -> Bool

  each_ : Array a -> (a -> IO ()) -> IO ()
```

### Byte Arrays

```ipsorepl
> :kind Bytes
Type
```

### Records

#### Construction

```ipsorepl
> :t { x = 1, y = true }
{ x : Int, y : Bool }
```

```ipsorepl
> a = 1
a : Int
> b = true
b : Bool
> { a, b }
{ a = 1, b = True }
```

#### Projection

```ipsorepl
> x = { a = "hello", b = false }
x : { a : String, b : Bool }
> x.a
"hello"
> x.b
false
```

#### Extension

```ipsorepl
> rest = { more = false, words = ["a", "b"] }
rest : { more : Bool, words : Array String }
> :t { some = "some", ..rest }
{ some : String, more : Bool, words : Array String }
```

### Variants

#### Construction

```ipsorepl
> :t None
forall r. (| None, r |)
```

#### Extension

```ipsorepl
> :t \x -> (| A, ..x |)
(| r |) -> (| A : a, r |)
```

### IO

```ipsorepl
> :kind IO
Type -> Type
```

```ipsorepl
> comp
.   bind line <- readln
.   print line
hello
hello
```

#### Builtins

```ipso
module io where
  
  pure : a -> IO a

  map : (a -> b) -> IO a -> IO b

  andThen : IO a -> (a -> IO b) -> IO b
```

```ipso
println : String -> IO ()

print : String -> IO ()
```

```ipso
readln : IO String
```

### Commands

```ipsorepl
> :kind Cmd
Type
```

```ipsorepl
> :type `echo "hello, world!"`
Cmd
```

#### Builtins

```ipso
module cmd where

  run : Cmd -> IO ()

  # Run a command and return its exit status.
  try : Cmd -> IO (| Success : (), Failure : Int |)

  # Run a command, capturing its `stdout` as a string.
  read : Cmd -> IO String
  
  # Run a command, capturing the lines it writes to `stdout`.
  #
  # `lines command` is equivalent to `io.map (string.splitc '\n') (cmd.read command)`
  lines : Cmd -> IO (Array String)
 
  # Run a command and execute an `IO` action for each line written to `stdout`.
  #
  # `eachline_ cmd f` is equivalent to `io.andThen (cmd.lines cmd) (\lines -> array.each_ lines f)`
  eachline_ : Cmd -> (String -> IO ()) -> IO ()

  show : Cmd -> String
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

### Debugging

```ipso
class Debug a where
  debug : a -> String

instance Debug ()
instance Debug Bool
instance Debug Int
instance Debug Char
instance Debug String
instance Debug Cmd
instance Debug a => Debug (Array a)
instance DebugRecordFields a => Debug { a }
instance DebugVariantCtor a => Debug (| a |)
```

```ipso
debugRecordFields : DebugRecordFields a => { a } -> Array { field : String, value : String }
debugVariantCtor : DebugVariantCtor a => (| a |) -> { ctor : String, value : String }
```

## Standard Library

### `env`

```ipso
module env where

  # The current program's name.
  #
  # Example:
  # 
  # ```
  # $ cat > test.ipso <<EOF
  # main : IO ()
  # main =
  #   comp
  #     bind program <- env.program
  #     println program
  # EOF
  # $ ipso test.ipso -- a b c
  # test.ipso
  # ```
  program : IO String

  # The arguments passed to the current program.
  #
  # Example:
  # 
  # ```
  # $ cat > test.ipso <<EOF
  # main : IO ()
  # main =
  #   comp
  #     bind args <- env.args
  #     println <| debug args
  # EOF
  # $ ipso test.ipso -- a b c
  # ["a", "b", "c"]
  # ```
  args : IO (Array String)
  
  getvar : String -> IO (| Some : String, None : () |)
  
  getvar! : String -> IO String
  
  setvar : String -> String -> IO ()
```

### `exit`

```ipso
module exit where

  success : IO a

  failure : IO a

  with : Int -> IO a
```

### `file`

```ipso
module file where
  
  read : String -> IO String
  
  write : String -> String -> IO ()
  
  append : String -> String -> IO ()
```

### `path`

```ipso
module path where

  exists : String -> IO Bool
```

## Grammar

```
ident ::=
  ident_start ident_continue*
  
ident_start ::= (lowercase ASCII alphabetic characters)

ident_continue ::= 
  (ASCII alphanumeric characters) |
  '_' |
  '!' |
  '?'


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
  pattern_atom |
  ctor pattern_atom

pattern_atom ::=
  ident |
  int |
  char |
  string |
  '{' [record_pattern] '}' |
  '_' |
  '(' [pattern] ')'
  
record_pattern ::=
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

cmd_string_part ::=
  cmd_char+
  '$' ident
  '$' '{' expr '}'

cmd_part ::=
  cmd_string_part+
  '$..' ident
  '$..{' expr '}'

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
</div>
</div>
