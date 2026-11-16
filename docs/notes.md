# SLIP Notes

- **multipass** or utm for linux

- code - read compile
 - read as symbols, lists, etc
 - convert list to functions
  - how to deal with macros that evaluate later?
 - evaluate
  - could or should this be done in compile stage? - no, need to look forward to future defuns in file
 - build test for tracing various bits of code
  - verify each phase
 - for dummy func, have to figure out what package it was in
  - without a package in the func name maybe a any package place holder
 - for all functions create a place holder with no package
  - resolve to package dummy on eval
   - what about functions that change packages?
 - two phases
  - read to lists of symbols and primitives
  - eval
   - for defun leave body as list of symbols until called
    - for symbols, if func, add dummy to current package
  -  sbcl can not call func before defined in a file
  - with compile a no op, what about provenance?

- bugs
 - read with in-package has order problem
  - read without any eval to create code
  -
 - package private
 - make sure defparameter can be exported
 - bugs dir

---------------------

- rethink read/compile/eval cycle
 - try and remove special case for defxxx functions and in-package
 - maybe postpone as much as possible, keep psuedo functions (undefined) around but have them be able to resolve
 - how about first pass, just lists
  - second pass convert to functions (maybe combine with first)
  - third call each including defuns
   - calls need to resolve functions in this step

---------------------

- make coverage interactive
 - functions for terminal support?
  - with-raw-terminal (term)
   - term is an instance
    - methods
     - :draw-frame
     - :write (string &key left top right bottom)
     - :write-at (string left top)
     - :clear
  - need input channel as well
  - functions for terminal support?
   - add term or termio package using some stuff from the repl
    - with-raw-terminal (term)
     - recover and go back to original
     - can it be run from repl? maybe not
     - color
     - move
     - clear
     - etc
  - or coverage viewing as part of repl?

---------------------

- flavor allow out of order defflavor like standard-class

---------------------

- for later
 - [ ] WITH-ACCESSORS
 - [ ] ENSURE-GENERIC-FUNCTION
 - [ ] REINITIALIZE-INSTANCE
 - [ ] UPDATE-INSTANCE-FOR-REDEFINED-CLASS
 - [ ] CHANGE-CLASS - call generic update-instance-for-different-class

  - clos https://lispcookbook.github.io/cl-cookbook/clos.html and https://www.algo.be/cl/documents/clos-guide.html

-----------------
  - [ ] inspect [interactive]
   - inspect data, break down at each level
   - repl but with extra bindings for numbers

 - tough-ones
  - [ ] DESTRUCTURING-BIND
  - [ ] FORMATTER
  - [ ] INTEGER-DECODE-FLOAT

  - allow (coerce '(1 0 1 0) '(vector (integer 0 1) 4)) - in the future
   - might need a type-spec type to use
    - TypeSpec interface
     - check method that panics on fail
     - isOk method for true or false
      - separate for each type like integer, float, etc
       - IntegerSpec - low, high

 - watch.connect.safeEval
  - encode condition and decode
  - frame test broken

 - package-export
  - import
  - shadow
  - package-shadowing-symbols
  - shadowing-import


- flow state
 - need a way to identify new or existing
 - have to identify service and id
 - if a flow is the remote service then ...
  - identify flow
  - attach to monitor
   - how are flows monitored
    - add event publishing to tracing
     - nats ?


- future repl options
 - select, copy, yank, ring (stack) (is this really needed?)
  - need select
   - mark
   - highlight from mark to current
   - ^w to delete
   - M-w to copy
  - ^y for yank
  - M-y for next
  - need hookup with system copy and paste

- other method combinations?
 - :method-combination option for defflavor
 - daemon (default)
 - progn
 - or
 - and
 - list

- ui with fyne
