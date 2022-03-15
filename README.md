# slip

SLIce Processing is LISP for golang



-------------------------------------------------------------------------------

- tests for base types
 - test tool for testing object types
  - maybe helpful for others as well (what package?)
   - sliptest
    - TestObjec{obj Object, str string, simple interface{}, hierarchy []string, eq map[Object]bool}
     - hierarchy could be a single string like foo.bar.t

- need panic wrapper
 - depends on stack
- add remaining base type
 - hash-table map[Object]Object
 - ratio
 - bignum
 - complex
 - stream
 - array
 - stream
  - socket?
 - class
 - instance
 - function
 - macro
 - macrog - golang macro

- world
 - refVars
  - set and get functions to set and get
   - check after regular vars don't match

- function
 - how to return multiple values
  - (values-list '(1 2))
   - special kind of list
   - unless wrapped in a function that handles multiple-values the first value is returned
    - how to do that, need knowledge of outer function
    - or maybe all have to check, have to check types anyway so...

- types
 - function
 - array (multi demensional) #<ARRAY 4x3 simple 32546632> of #2A((1 2 3)(2 3 4)) of #3A or ...
  - if (make-array '(1) then it is a vector #(1 2 3)
  - (aref foo 1 2)
 - struct (defstruct)
 - class
  - instances
 - ratio (struct{ num Object, denom Object}
 - stream
  - file
  - socket
   - pipe
 - simple (json-ish, from ojg)
  - format as json or sen

 - fork

- lisp - SLIce Processing (slip)
 - name
  - gosp
  - gisp
  - glisp
  - golisp - taken
  - lispy
  - slice processing (sp) (slip)
  - list processing (lp)
 - list is reverse slice
 - support marcos
  - represented as scoped fun
  - support macro chars of , and ,@ in a backquoted (not single quote) list
   - , eval
   - ,@ is like: if foo is (a b) the ,@foo becomes a b
   - ', as is, no eval, (as string?)
   - https://lisp-journey.gitlab.io/blog/common-lisp-macros-by-example-tutorial/
   - defmacro defined should expand at compile time
   - golang macro should just not eval args

 - handle args of &rest and :key
 - what to call sexpr/**object**
 - object & class support
 - should a stack be used for eval instead of nested function calls?
