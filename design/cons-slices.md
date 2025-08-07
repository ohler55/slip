# Cons versus Slices

Most LISP implementations are built on a structure called a __cons__
which is composed of a __car__ and a __cdr__. Lists are composed of a
linked list of __cons__ cells. A list of `(1 2 3)` is composed of
three __cons__ cells.

```
[car|cdr]
  ▼   ▼
  1 [car|cdr]
      ▼   ▼
      2 [car|cdr]
          ▼   ▼
          3  nil
```

SLIP is based on go slices instead of __cons__ cells. The same list,
`(1 2 3)` is:

```
[1][2][3]
```

A __cons__ cell in SLIP is a list of two elements with a special
_tail_ structure used when a __cons__ `(1 . 2)` is expected such as in
an association list.

```
[1][tail{value:2}]
```

## Reasons

There are a number of reasons for using slices instead of a `Cons`
struct with a `Car` and `Cdr` fields. The decision to use slices
does come with sacrifices.

### Memory

In terms of overall memory use slices require less memory to represent
the same list. With an implemenation that uses a separate stuct
instance for each element of a list there is the overhead of then
struct plus then pointer to that struct and then associated type
reference. A slice on the otherhand is one chunk of memory that is a
list of references to values. For lists of only a couple of elements
then differences minimal but for a list of hundreds or hundreds of
thousands the difference is more significant. In addition lots of
small cons struct instances leads to fragmented memory and a higher
load on then garbage collector.

### Performance

Benchmarks were used to evaluate the differences in performace between
then use of cons structs and slices. In most use case slices performed
better in both memory use and in elapsed time. There were exceptions
though. For example prepended a value to a list `(cons x list)` was
much slower using slices when appending to very long lists. In other
cases such as iterating over a list then slice approach had the
advantage. In the case of `(cons x list)` a change in approach to
append to a list instead more than compensated for the performance
difference.

### Sacrifices

- loops
- cons to a list, copied code versus new code
