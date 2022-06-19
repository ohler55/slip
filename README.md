# SLIP

SLIce Processing is LISP for golang



-------------------------------------------------------------------------------

- next
 - test coverage
 - #'
  - test with %s

- simple
 - separate package
  - simple
 - use flavors for the instance and methods or just functions
  - is there a need for daemons? if not then maybe not worth making an object unless for name spacing
 - functions or methods
  - format as json or sen
  - get with mapc like interface, maybe get and collect (getc?)
  - simple-first
  - simple-set
  - simple-has
  - simple-parse-json
  - simple-parse-sen
  - make-json-path
  - make-simple
  - json-path-p
  - simple-p
  - simple-walk
  - simple-to-lisp

 - other method combinations?
  - :method-combination option for defflavor
  - daemon (default)
  - progn
  - or
  - and
  - list

- would building our own stack be a better approach?
 - could reuse scope but would need to clear vars
 - could replace args key lookups for lispcaller with an index to arg to avoid using maps

- Code
 - #' - function (like quote)
 - #. is read time eval of object if *read-eval* is true else panic

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

- functions
 - pkg
  - basic
   + quote
   + set
   + setq
   + setf
   - let
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
   - intern (string to symbol)
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
 + complex
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

- Placer
 - place value (car, cdr, nth, rest, first, card, aref, symbol)
  - others in hash-table, array, vector

- load
 - compile and then eval

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

- support marcos
 - represented as scoped fun
 - support macro chars of , and ,@ in a backquoted (not single quote) list
  - , eval
  - ,@ is like: if foo is (a b) the ,@foo becomes a b
  - ', as is, no eval, (as string?)
  - https://lisp-journey.gitlab.io/blog/common-lisp-macros-by-example-tutorial/
  - defmacro defined should expand at compile time
  - golang macro should just not eval args
