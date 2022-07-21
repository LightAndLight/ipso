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

```ipso
greet : IO ()
greet =
  comp
    bind you <- cmd.run `whoami`
    println "Hey, $you"
```
</div>
</div>