# slip

SLIce Processing is LISP for golang



-------------------------------------------------------------------------------


- Code
 - #' - function (like quote)
 - #. is read time eval of object if *read-eval* is true else panic

- cmd/slip
 - on load read then compile then eval

- code.Compile
 - turns lists into functions
  - replace in code to allow second eval (if defuns then treat as if called a second time)
 - defuns evaluated first if at top level
  - when compiled check function is defun (or defmacro later) if so then eval
   - maybe first pass to defun then second pass for all others
    - misses defun inside another function but too bad

- LispCaller or FormCaller
 - change Dynamic to point to a LispCaller
  - put Doc and Forms in caller
  - for defun put LispCallers in a map
  - create create function when lisp caller is defined (either implicitely or with defun)

 - createFunc
  - create function with self pointer to lisp caller

 - how different than dynamic?
  - can they be the same with some changes?
  -


 - functions
 - shadows
 - uses (packages)
 - package struct
  - name
  - aliases
  - vars
  - functions
  - uses []*Package
   - need to mask shared vs not
    - how is that defined? Just by using package?
   - use export on package define
 - map of all packages (with nicknames or aliases)
 - default package global or a package
 - is a double lookup needed for all vars?
  - export then package vars unless *package* is same package
  - check current package first then uses

 - var visibility
  - from package
   - same package vars pkg.var map
   - imported vars from any package, redirect (same as uses)
   - exported from uses packages unless shadowed
    - PkgUse { pkg, vars (point to something or just a check for a second lookup?), }
   - any package with pkg:var
    - check pkg exports (external) first then get var if ok
   - non-exported (internal) from any with pkg::var
    - direct to pkg.var map
   - intern and unintern
    - adds symbol to package (same as defvar ?)
  - get
   - current pkg map
   - current pkg imports map
   - used pkg
    - exports check then map (unless var is a wrapper, double map lookup might be best)
 - all :zzz are in the keyword package ans implicitly used
 - for exports from CL maybe a wildcard like a nil export list
 - how to deal with function code
  - when function runs it has all the vars in the package it was defined in and also *package*
   - that means it need ref to it's package

  type Import struct {
    pkg *Package
    name string
  }
  type Used struct {
    pkg *Package
    imports map[string]*Import (or maybe just map[string]bool
  }
  type Package struct {
    vars map[string]Object
    imports map[string]*Import
    used []*Used
  }



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

- steps
 - parse from bytes to lists/objects
 - compile evaluates defun and macros
  - is that the same as eval?
   - maybe update lists to functions on the first pass
 - eval
 - maybe
  - parse into Code
  - Code.Eval()
   - eval does list to function conversions
   - deal with defuns as well as calling functions
    - maybe a separate call for repeated calls like Run()
  - maybe a compile that only runs defun, defmacro, defvar, etc
  - then an eval, run, or exec for code that will be run multiple times


- compile
 - just parse into lists?
  - second pass to convert symbols to functions


 - just compile
  - don't eval defun
  - keep as list of Objects/functions with args (maybe a progn?)
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
