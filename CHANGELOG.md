# Changelog

## 0.6

*2023-06-01*

### Breaking changes

* `array.index` is now called `array.get!` ([#386](https://github.com/LightAndLight/ipso/issues/386))

### Features and additions

* New builtin functions
  * `eprintln : String -> IO ()` ([#409](https://github.com/LightAndLight/ipso/issues/409))
  * `eprint : String -> IO ()` ([#409](https://github.com/LightAndLight/ipso/issues/409))
  * `dprintln : Debug a => a -> IO ()` ([#409](https://github.com/LightAndLight/ipso/issues/409))
  * `dprint : Debug a => a -> IO ()` ([#409](https://github.com/LightAndLight/ipso/issues/409))
  * `array.get : Int -> Array a -> (| Some : a, None : () |)` ([#386](https://github.com/LightAndLight/ipso/issues/386))
  * `array.first : Array a -> (| Some : a, None : () |)` ([#376](https://github.com/LightAndLight/ipso/issues/376))
  * `array.last : Array a -> (| Some : a, None : () |)` ([#376](https://github.com/LightAndLight/ipso/issues/376))
  * `array.swap : Int -> Int -> Array a -> Array a` ([#325](https://github.com/LightAndLight/ipso/issues/325))
  * `array.filter : (a -> Bool) -> Array a -> Array a` ([#321](https://github.com/LightAndLight/ipso/issues/321))
  * `array.filterMap : (a -> (| Some : b, None : ()|)) -> Array a -> Array b` ([#321](https://github.com/LightAndLight/ipso/issues/321))
  * `array.find : (a -> Bool) -> Array a -> (| Some : Int, None : () |)` ([#324](https://github.com/LightAndLight/ipso/issues/324))
  * `array.findMap : (a -> (| Some : b, None : () |)) -> Array a -> (| Some : b, None : () |)` ([#324](https://github.com/LightAndLight/ipso/issues/324))
  * `int.min : Int -> Int -> Int` ([#394](https://github.com/LightAndLight/ipso/issues/394))
  * `int.max : Int -> Int -> Int` ([#394](https://github.com/LightAndLight/ipso/issues/394))
  * `int.parseBin : String -> (| Some : Int, None : () |)`([#393](https://github.com/LightAndLight/ipso/issues/393))
  * `int.parseOct : String -> (| Some : Int, None : () |)`([#393](https://github.com/LightAndLight/ipso/issues/393))
  * `int.parseDec : String -> (| Some : Int, None : () |)`([#393](https://github.com/LightAndLight/ipso/issues/393))
  * `int.parseHex : String -> (| Some : Int, None : () |)`([#393](https://github.com/LightAndLight/ipso/issues/393))
  * `string.toChars : String -> Array Char` ([#318](https://github.com/LightAndLight/ipso/issues/318))
  * `string.fromChars : Array Char -> String` ([#318](https://github.com/LightAndLight/ipso/issues/318))
  * `string.startsWith : String -> String -> Bool` ([#319](https://github.com/LightAndLight/ipso/issues/319))
  * `string.stripPrefix : String -> String -> (| Some : String, None : () |)` ([#319](https://github.com/LightAndLight/ipso/issues/319))

### Improvements and fixes

* Allow literal backslashes in string literals ([#395](https://github.com/LightAndLight/ipso/issues/395))

## 0.5

*2023-02-26*

### Breaking changes

* Calling `string.parts` with an empty delimiter results in a runtime error ([#359](https://github.com/LightAndLight/ipso/issues/359))

  Before this change, `string.parts "" input` looped forever for non-empty values of `input`. It
  returned `""` when `input` was empty. Now the function exits with a runtime error when it's called
  with an empty delimiter, regardless of the `input`:

  ```ipsorepl
  > string.parts "" "a"
  error: string.parts called with empty delimiter
  ```

### Features and additions

* Relax indentation in record and variant types ([#365](https://github.com/LightAndLight/ipso/issues/365))

  You can now use "Java-style indentation" for large record and variant types in type signatures.
  For example:

  ```ipso
  test :
    String ->
    {
      a : {
        b : (),
        c : (),
        d : ()
      },
      b : (|
        A : (),
        B : (),
        C : (),
        D : ()
      |)
    }
  test = ...
  ```

  [Line joining rules](https://ipso.dev/docs/reference.html#line-joining) still apply, so code like
  this:
  
  ```ipso
  test : {
    a : (),
    b : (),
    c : ()
  }
  test = ...
  ```

  is invalid.


### Improvements and fixes

* Allow empty string patterns ([#356](https://github.com/LightAndLight/ipso/issues/356))

  Pattern matching on string literals was already allowed, but I forgot to support the empty string.

  ```ipso
  case x of
    "" -> "it's empty"
    "a" -> "it's a"
    "b" -> "it's b"
    _ -> "it's not empty, a, or b"
  ```

* Improve type errors in function arguments ([#377](https://github.com/LightAndLight/ipso/issues/377))

  Type errors in function arguments are now more accurate in some situations. See the issue for an
  example.

#### Bug fixes

* Fix a panic due to an undesugared pattern ([#358](https://github.com/LightAndLight/ipso/issues/358))
* Fix lambda pattern elaboration ([#357](https://github.com/LightAndLight/ipso/issues/357), [#326](https://github.com/LightAndLight/ipso/issues/326))
* Fix scope management in interpreter ([#362](https://github.com/LightAndLight/ipso/issues/362))
* Fix name resolution in submodules ([#189](https://github.com/LightAndLight/ipso/issues/189))

## 0.4

*2023-02-13*

### Breaking changes

* `String` interpolation in commands with `$` ([#138](https://github.com/LightAndLight/ipso/issues/138))

  To simplify <a href="#cmd-literal-quoted-string-interpolation">string interpolation in quoted
  command literal arguments</a>, `$` now only substitutes `String` values instead being overloaded
  by `ToArgs`.

  Use <a href="#cmd-argument-splatting">command argument splatting</a> with `$..` to substitue an
  array of arguments.

* Removed `ToArgs` class ([#348](https://github.com/LightAndLight/ipso/issues/348))

  Command argument interpolation uses `$` to substitute `String`s
  ([#138](https://github.com/LightAndLight/ipso/issues/138)) and `$..`
  ([#346](https://github.com/LightAndLight/ipso/issues/346)) to substitute
  `Array String`s. `ToArgs` is no longer needed to overload `$`.

### Features and additions

#### `ipso` CLI

* `--check` flag ([#312](https://github.com/LightAndLight/ipso/issues/312))

  `ipso --check FILE` parses and type checks a file without running it. It either
  exits successfully (with exit status 0) or prints the relevant error messages
  and exits with a failure status.

* Arguments can be passed to script without `--` ([#316](https://github.com/LightAndLight/ipso/issues/316))

  This allows arguments to be correctly forwarded to scripts that have shebang lines:

  `test.ipso` 
  ```ipso
  #! /usr/bin/env ipso

  main : IO ()
  main = 
    comp
      bind args <- env.args
      println <| debug args
  ```

  ```bash
  $ ./test.ipso --help
  ["--help"]
  ```

#### Language

* Array pattern matching ([#81](https://github.com/LightAndLight/ipso/issues/81))

  You can pattern match on arrays in function definitions and `case` expressions:

  ```ipso
  f [a, b, c] = a * b * c

  case x of
    [] -> "zero"
    [x] -> "one"
    _ -> "many"
  ```

  Array patterns can only contain variables; nested patterns in arrays aren't supported
  yet. Progress is tracked in [#354](https://github.com/LightAndLight/ipso/issues/354).

* <a name="cmd-argument-splatting">Command argument splatting</a> ([#346](https://github.com/LightAndLight/ipso/issues/346))

  Use `$..` to substute `Array String`s into commands, where each item becomes a separate
  command argument:

  ```ipsorepl
  > :t \x -> `echo $..x`
  Array String -> Cmd
  
  > `echo $..{ ["a", "b", "c d"] }
  `echo a b "c d"`
  ```

* <a name="cmd-literal-quoted-string-interpolation">String interpolation in quoted command literal arguments</a> ([#138](https://github.com/LightAndLight/ipso/issues/138))

  Expressions like

  ```ipso
  `echo "hello: $name"`
  ```

  are now allowed.

  Previously they were rejected as sytnax errors, and you'd have to work around it by writing

  ```ipso
  let arg = "hello: $name" in `echo $arg`
  ```

  instead.

* `!` and `?` in identifiers ([#286](https://github.com/LightAndLight/ipso/issues/286))

  `!` and `?` symbols are now allowed in identifiers:

  ```ipso
  bangbang! : IO ()
  bangbang! = println "my baby shot me down"
  ```

  ```ipso
  if empty?
    then "yes"
    else "no"
  ```

#### Builtins

* `not : Bool -> Bool` ([#322](https://github.com/LightAndLight/ipso/issues/322))
* `env.getvar! : String -> IO String` ([#285](https://github.com/LightAndLight/ipso/issues/285))
* `cmd.try : Cmd -> IO (| Success : (), Failure : Int |)` ([#327](https://github.com/LightAndLight/ipso/issues/327))
* `string.trimp : (Char -> Bool) -> String -> String` ([#256](https://github.com/LightAndLight/ipso/issues/256))
* `string.trimc : Char -> String -> String` ([#256](https://github.com/LightAndLight/ipso/issues/256))
* `string.trim : String -> String` ([#256](https://github.com/LightAndLight/ipso/issues/256))

### Improvements and fixes

#### Language

* Use escaped `$` symbol in command literals ([#335](https://github.com/LightAndLight/ipso/issues/335))

  Expressions like

  ```ipso
  `echo \$`
  ```

  were incorrectly rejected as syntax errors.

  Here's what happens now:

  ```ipsorepl
  > cmd.run `echo \$`
  $

  ```

#### CI / Build

* Add GitHub actions cache for `.cargo` directories ([#294](https://github.com/LightAndLight/ipso/issues/294)) 
* Share GitHub actions caches across branches ([#338](https://github.com/LightAndLight/ipso/issues/338))
* Report errors for unused crate dependencies ([#126](https://github.com/LightAndLight/ipso/issues/126))

## 0.3

*2022-11-24*

### Breaking changes

* Command line argument access from scripts ([#255](https://github.com/LightAndLight/ipso/issues/255))

  You can pass arguments to an ipso script by adding `--` to the command:

  ```bash
  ipso script.ipso -- a b c
  ```

  `env.program` and `env.args` provide access to the script/program name
  and the command line arguments. See the [`env` module
  documentation](https://ipso.dev/docs/reference.html#env) for usage examples.

* `string.split` takes a `String` delimiter ([#277](https://github.com/LightAndLight/ipso/issues/277))

  We now have `string.split : String -> String -> Array String` so that
  `string.join` is a better inverse. To split with a `Char` as a delimiter, use [`string.splitc`](#string-splitc)

* Fixed incorrect record evaluation ([#300](https://github.com/LightAndLight/ipso/issues/300))

### Features and additions

* Nested patterns ([#95](https://github.com/LightAndLight/ipso/issues/95))

  You can now pattern match through multiple levels of a datatype in a single pattern:

  ```ipso
  case x of
    C (C (C a)) -> a
  ```

  This desugars to

  ```ipso
  case x of
    C x1 ->
      case x1 of
        C x2 ->
          case x2 of
            C a -> a
  ```

  which is what you'd have written by hand without this feature.

* Unit patterns ([#303](https://github.com/LightAndLight/ipso/issues/303))

  Pattern match on the unit type:

  ```ipso
  case x of
    () -> ...
  ```

  This works nicely with variants that have "nullary" constructors (constructors whose argument is
  `()`):
  
  ```ipso
  orElse : (| Some : a, None : () |) -> a -> a
  orElse option default =
    case option of
      Some a -> a
      None () -> default
  ```

* New builtins: `string.parts` and `string.partsc` ([#280](https://github.com/LightAndLight/ipso/issues/280))

  Like `string.split` and `string.splitc`, but more convenient when you
  don't want to preserve delimiter locations.

  Documentation [here](https://ipso.dev/docs/reference.html#builtins-2).

* `Debug` instance for `Cmd` ([#268](https://github.com/LightAndLight/ipso/issues/268))

* <span id="string-splitc">New builtin: `string.splitc` ([#277](https://github.com/LightAndLight/ipso/issues/277))</span>

### Improvements and fixes

* Fixed some pattern match error positions ([#273](https://github.com/LightAndLight/ipso/issues/273))

## 0.2

*2022-10-30*

### Breaking changes

* Duplicate top-level definitions are no longer allowed ([#222](https://github.com/LightAndLight/ipso/issues/222))

* The newline character (`U+000A`) is no longer allowed in string and command literals ([#216](https://github.com/LightAndLight/ipso/issues/216))

### Features and additions

* Expression interpolation in command literals ([#224](https://github.com/LightAndLight/ipso/issues/224))

  Example:

  ```ipsorepl
  > `echo ${ string.join "," ["a", "b", "c", "d"] }`
  `echo a,b,c,d`
  ```
  
* `array.each_` ([#211](https://github.com/LightAndLight/ipso/issues/211))
  
  ```
  each_ : Array a -> (a -> IO ()) -> IO ()
  ```

* `cmd.eachline_` ([#211](https://github.com/LightAndLight/ipso/issues/211))

  ``` 
  eachline_ : Cmd -> (String -> IO ()) -> IO ()
  ```

### Improvements and fixes

* Better type errors ([#218](https://github.com/LightAndLight/ipso/issues/218))

* In command literals, adjacent expression and string parts are now
  considered part of the same argument ([#215](https://github.com/LightAndLight/ipso/issues/215))


  Example:

  ```ipsorepl
  > let a = "one" in
  . let b = "two" in
  . `echo $a/$b`
  `echo one/two`
  ```

* Recursive type class instances now work ([#19](https://github.com/LightAndLight/ipso/issues/19))

* Single-line comments can no longer cause parse errors ([#188](https://github.com/LightAndLight/ipso/issues/188))

## 0.1

*2022-08-06*

Initial release 🎉