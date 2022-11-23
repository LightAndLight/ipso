# Changelog

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