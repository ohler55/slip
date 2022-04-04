# slip

SLIce Processing is LISP for golang



-------------------------------------------------------------------------------

- float
 - should float be an interface? - yes
  - long-float (big.Float)
  - double-float (float64)
   - short-float and single-float are just aliases for double-float
 - rename Float to DoubleFloat
  - add most-positive-double-float as well as short and single versions
  - add most-negative-double-float as well as short and single versions
  - add least-negative-xxx-float
  - add least-positive-xxx-float
  - add long-float-digits
  - add xxx-float-epsilon and xxx-float-negative-epsilon (same values)
  - need option for printing s, f, d, l instead of e
   - honor *print-case* for case of exponent letter
   - use *print-readably* to determine e vs other
 - plan
  - make a Float interface like integer
  - create long-float
   - along with globals
  - add *print-prec* with ... if more digits past prec
  - maybe short-float as float32 - might be useful for vectors to save space


- use (type-of x) or (typep x 'long-float)

- how to handle marco characters in compile
 - backquote ` of list allow use of , options
  - , eval
  - ,@ is like: if foo is (a b) the ,@foo becomes a b
  - ', as is, no eval, (as string?)
  - maybe treat like (quote foo) but (backquote (foo bar))
   - (comma x)
   - (comma-at x)
   - (quote-comma x)
 - or maybe expand to normal functions
  - (foo ,bar) => (list 'foo bar)

- dynamic.go
 - return-from function

- lambda
 - eval of lambda should return a function

- defun

- functions
 - pkg
  - basic
   + quote
   + set
   + setq
   + setf
   - describe
   - apropos
   - null
   - atom
   - eq
   - equal
   - defvar
   - defun - create a named Dynamic
   - lambda - create a Dynamic
   - defmacro
   - coerce
  - list
   + car
   + cdr
   + cadr
   - cons
   + list
   - nth
  - number
   - incf
   - decf


- types
 + ratio
 + bignum
 + float
  + long-float -> big.Float
  + double-float
  + single-float (alias short-float)
 - complex
 - stream
  - socket?
  - string
   - with-input-from-string
   - make-string-input-stream
   - with-output-to-string
   - make-string-output-stream
   - get-output-stream-string
   - write-line
   - write-string
   - write-char
   - read-line
   - read-char
 - class
 - instance
 - function
 - macro
 - macrog - golang macro

- Placer
 - place value (car, cdr, nth, rest, first, card, aref, symbol)
  - others in hash-table, array, vector

- compile
 - just compile
  - don't eval defun
  - keep as list of functions with args (maybe a progn?)
   - maybe list of objects since last maybe be the return value
 - eval the list of functions

- load
 - compile and then eval

- scope
 - eval
  - should functions eval functions instead of having scope do the arg evals?
  - do functions always need a scope for the args?
   - yes for lambdas and as args
    - could just create scope when calling lambdas
   - no at first, only if lambda is there
  - options
   - scope evals args
   - fun evals args
  - trace
   - set on scope or global?
   - if depth is always given then if >= 0 trace
    - maybe functions to display in and out
     - call in func after eval of args
     - call out func on exit or in defer?
      - maybe scope eval can do this
   - if trace is set in scope then the set is either trace func or no-op


- function
 - how to return multiple values (slip.Values)
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
