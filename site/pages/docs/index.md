---
title: Docs
---

# Documentation

## Installation

You can download the latest release from [GitHub](https://github.com/LightAndLight/ipso/releases/latest).

Nix users can access `ipso` via a Nix flake:

```
# Use the repo's default branch
$ nix shell github:LightAndLight/ipso

# Access a specific release
$ nix shell github:LightAndLight/ipso/v0.1-rc.2
```

## Your First Program

```
$ cat > hello.ipso <<EOF
main : IO ()
main = println "Hello from ipso!"
EOF

$ ipso hello.ipso
"Hello from ipso!"
```

## Further Reading

* [Language Reference](./reference.md)
