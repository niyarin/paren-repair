# Paren repair
This software corrects missing or extra parentheses using beam-search–based scoring in R{7,6,5}RS Scheme source code.
It repairs parentheses by considering not only indentation but also syntactic correctness, evaluated via a pushdown automaton.


## Build
```sh
chicken-install r7rs
chicken-install srfi-113
make
```

## Usage
After build
```sh
./bin/paren-repair test-resources/bad-let1.scm

```
