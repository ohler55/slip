# FLOS

FLOS is a merging of **Fl**avors and CL**OS**. As a means to reuse
Flavors methods using a function notation FLOS was developed.

As mentioned in [flavors.md](flavors.md), Flavors was developed
first. With generics function names are always in the global name
space so care must be taken when importing packages to avoid name
collisions. That aside, generics are the modern approach to objects in
Common LISP.

FLOS is a half step toward generics. Methods on Flavors can be wrapped
with a ordinary function when building an API with go code.

The approach taken is to reuse the method along with the documentation
of the method and the arguments expected in calls to both the method
with a __send__ and for the function call. Since the wrapper calls a
__send__ under the covers the approach is similar to a generic
function with only one specializer that is the instance target of the
__send__.
