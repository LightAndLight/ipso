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

* Extensible [records](reference.html#records-1) and [variants](reference.html#variants-1)
* [Pattern matching](reference.html#pattern-matching)
* [String interpolation](reference.html#strings)
* [Command literals](reference.html#command-literals)
* [Computation expressions](reference.html#computation-expressions)
* [Type classes](reference.html#type-classes)
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