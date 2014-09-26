
General
=======

- Split out FromJSON instances from Definition
- Deal with values (e.g., description) that can either be a string, or an
  associative array.
- Figure out how a parser's options interact with subparser options
    * (MF: Maybe all global options must be specified before the subparser?)

Writers
=======
In rough order of priority:

- Bash completion
- Zsh completion
- Haskell
- Python
- Ruby
- C/C++
- Java
- Go
- Rust
- Fish completion
- Csh completion

Reader
======

Investigate the feasibility of generating a partial Garnett YAML file by
parsing usage string.
