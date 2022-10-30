# Changelog

## 0.2

*2022-10-30*

### Breaking changes

* Duplicate top-level are no longer allowed ([#222](https://github.com/LightAndLight/ipso/issues/222))

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