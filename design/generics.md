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
an ordinary function. The difference is how then forms of the
functions are evaluated.

## Generic Function Structure

- Aux added to funcinfo
 - Aux structure and use

- leverage method struct from flavors
 - could have been more specialize but a fair amount of overlap and names are the same

## Generic Function Evaluation

- cache methods
 - defmethod invalidates cache
- build the cached method

## `make-load-form`

- pretty-print and snapshot uses progn
