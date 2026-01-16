# Paren repair
Fixing broken parentheses in R{7,6,5}RS Scheme source code.

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
