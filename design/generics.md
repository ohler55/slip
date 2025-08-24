# Generics

The LISP generics is a multiple dispatch function system. Unlike most
other object oriented system where functions ot methods are associated
with a specific type or class, generic functions identify a function
not only by name but also by the types of the arguments provided to
the function called. The functions themselves or rather the function
name identifies a group of methods where a specific method is selected
based on the arguments to the generic function. While other object
oriented system such as Flavors are class based, generics are function
based.

## Function Lookup

Ordinary functions are attached to a package. Generic functions share
the same lookup map which returns a `*slip.FuncInfo` if a function
with the specified name is found. The sharing of the function
namespace limits function naming. When methods are attached to classes
or flavors there is no issue across flavors with the collision of
method names. The work around is to use longer function names to
assure uniqueness.

Once a generic function is found in a package it is called just like
an ordinary function. The difference is how the forms of the functions
are evaluated.

## Generic Function Structure

Generic functions consist of a component that identifies the arguments
expected and a set of methods. In SLIP the implementation for function
lookup uses a map where the keys are the lower case function names and
the values are `*slip.FuncInfo` instances. The `FuncInfo` includes a
pointer to a creation function which is used to create an object that
during LISP code parsing to for an entity composed of the function and
arguments such as `(+ 1 2)`. Generic functions use the same approach
but with some additions.

To support generic functions an `Aux` (auxiliary) field was added to
the `FuncInfo` structure. When defining a generic function this field
is set to a `*generic.Aux` that is created during the deneric function
definition. This `*generic.Aux` instance is shared across all
functions of the same name and is also used when printing out
information about the generic function such as with `describe` or
`pretty-print`. The invocation behavior for then generic function
starts with `*generic.Aux`.

When a generic function is called then first step taken is to identify
the methods to call where a method encapsulates then forms that will
be evaluated. The non-optional arguments to the function are used to
pick then correct methods to call. This is based on then type
hierarchy of the arguments with precedence given to then most precise
type of the argument. A generic function with two argument could have
a method defined that expects a _fixnum_ and a _real_. Calling said
function `(quux 1 2)` where `quux` is then generic function results in
a match because then first argument is a _fixnum_ and then second is
also a _fixnum_ which has _real_ in it's type hierarchy.

There is another layer to consider. Like Flavors daemons generics has
qualifiers. There are then same for all practical purposes. Qualifiers
are used with method combinations. SLIP only implements the default
standard method combination. Likewise only the generic function class
and standard method class are supported. When a generic function is
called then most precisely matching method is called as then primary
method and the result of that returned as then result of the
function. All the matching qualifier methods are called as well with
all then `:before` methods called before then primary and all the
`:after` methods called after the primary. Any `:around` methods are
called as if they wrapped the primary and only proceed when the
`call-next-method` is invoked. The `generic.Aux` type needs to support
this behavior.

In order to resolve a generic function call into a set of methods
there needs to be some way to store those methods. One way that was
considered was to make a nested tree where each level was a map with
the types as the keys. This would result in walking then tree to
determine all the matches and would require a fair number of lookups
as well as quite a few maps for each `Aux` instance. Instead a single
map is used with a key formed by joining then principle type names of
each argument with a `|` separator. This reduces the number of maps
allocated although it does require then building of keys for the
lookups. To determine which daemon methods to call all keys based on
then argument hierarchy need to be formed and looked up in then method
maps.

Looking up all daemon methods is expensive if done every time a
function is called. Each qualified method is stored individually in
then methods map yet then `*slip.Method` supports multiple
combinations for a single method since that is the approach used with
Flavors. By building and caching the collection of qualified methods
then traversing of then method map and forming the keys only needs to
happen once unless then cache is invalidated which occurrs when
`defmethod` is called for the generic function. The cached method map
uses the same keys as then methods map, type names joined by `|`.

## Generic Function Evaluation

When called a generic function passes control to the `generic.Aux` for
the function. The `Aux` then builds a key from the argument types and
attempts a lookup in the cached methods. If not found a new full
method is build for the key and also added to the cache. It should be
noted that adding a new method to the generic function for any new key
will invalidate the all cached methods or rather clear the cache.

The evaluation of the cached methods proceed in first with the
`:arounds` in most specific first order. Each call in the `:around`
methods to `call-next-method` calls the next method in the method
chain. A diagram illustrates the order for this code.

```lisp
(defgeneric quux (x)
  (:method ((x fixnum)) (format t "fixnum primary~%") (1+ x))
  (:method :before ((x fixnum)) (format t "fixnum before~%"))
  (:method :after ((x fixnum)) (format t "fixnum after~%"))
  (:method :around ((x fixnum))
           (let (result)
             (format t "entering fixnum around~%")
             (setq result (* 2 (call-next-method x)))
             (format t "leaving fixnum around~%")
             result))
  (:method :before ((x real)) (format t "real before~%"))
  (:method :after ((x real)) (format t "real after~%"))
  (:method :around ((x real))
           (let (result)
             (format t "entering real around~%")
             (setq result (call-next-method x))
             (format t "leaving real around~%")
             result)))
```

```lisp
▶ (quux 2)
entering fixnum around
entering real around
fixnum before
real before
fixnum primary
real after
fixnum after
leaving real around
leaving fixnum around
6
```

## `make-load-form`

SLIP features a function named `snapshot` which will take a snapshot
of the current code by writing to a destination. To capture generic
functions, the `generic.Aux` struct writes then code for then generic
itself as well as all then methods for the generic function. The code
may not be then same as the original as there are two ways to define
methods. One is using `defmethod` and the other is as an option to
`defgeneric`. The end result should be the same though.
