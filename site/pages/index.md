---
title: Home
no-edit: true
---

<div style="display: flex; flex-direction: row;">
`ipso` is a functional scripting language.

<div style="flex-grow: 1; margin-left: 2em;">
```ipso
main : IO ()
main = println "Hello world!"
```
</div>
</div>

<div style="display: flex; flex-direction: row;">
<div>
<h2 style="margin-top: 0em;">Features</h2>

* Extensible [records](./docs/reference.md#records-1) and [variants](./docs/reference.md#variants-1)
* [Pattern matching](./docs/reference.md#pattern-matching)
* [String interpolation](./docs/reference.md#strings)
* [Command literals](./docs/reference.md#command-literals)
* [Computation expressions](./docs/reference.md#computation-expressions)
* [Type classes](./docs/reference.md#type-classes)
</div>

<div style="flex-grow: 1; margin-left: 2em;">
```ipso
greet : IO ()
greet =
  comp
    bind you <- cmd.run `whoami`
    println "Hey, $you"
```
</div>
</div>