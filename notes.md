# SLIP Notes

- next

 - test package
  - defsuite
  - deftest

  - test type
   - set *current-test* when running
  - assert-equal
  - assert-match
  - refute-equal
  - assert-nil
  - refute-nil or assert-true
  - assert-panic
  - benchmark _form_ &key _iterations_ _duration_
   - evaluate for multiple times until enough time for run is good enough

 - conversions
  - to fixnum
  - to string
  - to float
  - others?

 - http package
  - flavors
   - server
    - start
    - shutdown
    - add-handler (path handler)
   - request
    - method
    - url
    - headers
    - body
    - trailers
    - remote-address
    - proto
   - response
    - write
    - add-header
    - :status
    - :headers
    - body
    - trailers
    - proto
   - some common base for request and response (http-message)
    -
   - handler
    - handle (req resp)
    - subclass for file and static pages (string)
  - client
   - get
   - put
   - post
   - etc

 - merge (starts with result-type)
 - room

 - watch (as in watch variables on a scope)
  - (defun watch (var-symbol &optional location) ...)
   - location is a list of horizontal and vertical postions
    - only works with editor (how to implement that?)
   - with no location simple print
    - incorporate with trace or is that a side effect?

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


- support marcos
 - represented as scoped fun
 - support macro chars of , and ,@ in a backquoted (not single quote) list
  - , eval
  - ,@ is like: if foo is (a b) the ,@foo becomes a b
  - ', as is, no eval, (as string?)
  - https://lisp-journey.gitlab.io/blog/common-lisp-macros-by-example-tutorial/
  - defmacro defined should expand at compile time
  - golang macro should just not eval args
