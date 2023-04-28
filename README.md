# SLIP

SLIce Processing is LISP for golang

TBD summary

## Almost Common LISP

TBD describe differences with what's missing and what's added

### Cons versus Slice

[car|cdr]
  1 [car|cdr]
      2 [car|cdr]
          3

```
┏━━━┳━━━┓
┃car┃cdr┃
┗━┳━┻━┳━┛
  ▼   ▼
  1 ┏━━━┳━━━┓
    ┃car┃cdr┃
    ┗━┳━┻━┳━┛
      ▼   ▼
      2 ┏━━━┳━━━┓
        ┃car┃cdr┃
        ┗━┳━┻━┳━┛
          ▼   ▼
          3  nil

┏━━━┳━━━┓
┃car┃cdr┃
┗━┳━┻━┳━┛
  ⛛   ⛛
  1 ┏━━━┳━━━┓
    ┃car┃cdr┃
    ┗━┳━┻━┳━┛
      ⛛   ⛛
      2 ┏━━━┳━━━┓
        ┃car┃cdr┃
        ┗━┳━┻━┳━┛
          ⛛   ⛛
          3  nil

[car|cdr]
  ▼   ▼
  1 [car|cdr]
      ▼   ▼
      2 [car|cdr]
          ▼   ▼
          3  nil

【car┃cdr】

【car|cdr】


```

### Missing Features

### Added Features

## Read and Eval

## REPL
