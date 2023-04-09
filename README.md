# SLIP

SLIce Processing is LISP for golang

-------------------------------------------------------------------------------


- data access packages
 - graphql
  - client
   - get back a bag
  - server
   - start up ggql server
    - resolve calls lisp
    - need to be thread safe
   - accept query using a lisp syntax
   - call lisp to fetch and populate bag for response
    - could be database query
    - or pull from multiple sources

- next
 - cl
  - do*
  - dolist
  - dotimes
  - return
   - store on scope?
   - return special type?
    - need value as well, maybe Return{From string, Value slip.Object}
    - in loop, check return of each
    - if return name matches current then return value else return Return up the call stack
  - loop (simple only)
  - prog
   - like let but with tags
   - maybe just treat the tags as symbols and (go tag) goes back or forward to tag
   - support return
   - starts like let
  - prog*
  - progn
  - merge (starts with result-type)
  - room
  - load - maybe handled in the compile phase as well as later)
  - gi:log - can this be made to work correctly (not collide with cl:log)?
  - tagbody
  - go
   - return a special goto type

 - package
  - support export list
   - ListToFunc should be f.ListToFunc
    - lookup of func should consider the package of f
     - if lookup is in f.pkg or lookup is exported (new flag) then ok
    - add export flag to FuncInfo
    - Define() and package.Define() need extra arg for export or not
     - maybe default to export
     - flag in FuncDoc or separate? depends on whether it's useful in docs


  - defpackage
  - in-package
  - package-name
  - package-nicknames
  - rename-package
  - shadow
  - package-shadowing-symbols
  - shadowing-import
  - intern
  - unintern
  - find-symbol
  - export
  - unexport
  - package-used-by-list
  - unuse-package
  - use-package
  - package-use-list
  - require
  - find-all-symbols
  - do-symbols
  - do-all-symbols
  - do-external-symbols

  - number
   - incf
   - decf

 - bag
  - remove
  - modify

 - defmacro


 - future repl options
  - color for words
   - functions - word after (
   - variables - match word
   - strings - quotes
   - comments - ;;
  - select, copy, yank, ring (stack) (is this really needed?)
   - need select
    - mark
    - highlight from mark to current
    - ^w to delete
    - M-w to copy
   - ^y for yank
   - M-y for next
   - need hookup with system copy and paste

 - coerce
  - also support integers unlike CL
  - only support symbols (later lists like '(integer 3 5) or '(and list (not null)) )


 - macro
  - expand on read

- base64 (encode and decode in gi package)

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
 - #. is read time eval of object if *read-eval* is true else panic

- use (type-of x) or (typep x 'long-float)
 - should work with flavors also (use heirachy)

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
   + let
   + describe
   + apropos
   - null
   - atom
   - eq
   - equal
   + defvar
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
