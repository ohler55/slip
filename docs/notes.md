# SLIP Notes

- **multipass** or utm for linux

---------------------

- add provenance

 - handle -f flag in app-args

 - coverage app
  - conditional (if when unless cond ...) (any with skip eval args)
   - update all that use EvalArg
     + cl/and.go
     + cl/block.go
     + cl/case.go
     + cl/cond.go
     + cl/defconstant.go
     + cl/defpackage.go
     + cl/defvar.go
     + cl/do-all-symbols.go
     + cl/do-external-symbols.go
     + cl/do-symbols.go
     + cl/do.go
     + cl/dolist.go
     + cl/dotimes.go
     + cl/dox.go
     - pkg/cl/ecase.go
     - pkg/cl/etypecase.go
     - pkg/cl/eval.go
     + cl/if.go
     - pkg/cl/ignore-errors.go
     - pkg/cl/let.go
     - pkg/cl/letx.go
     - pkg/cl/multiple-value-bind.go
     - pkg/cl/nth-value.go
     + cl/or.go
     - pkg/cl/prog.go
     - pkg/cl/progv.go
     - pkg/cl/progx.go
     - pkg/cl/psetf.go
     - pkg/cl/psetq.go
     - pkg/cl/return-from.go
     - pkg/cl/return.go
     - pkg/cl/rotatef.go
     - pkg/cl/setf.go
     - pkg/cl/setq.go
     - pkg/cl/shiftf.go
     - pkg/cl/tagbody.go
     - pkg/cl/the.go
     - pkg/cl/time.go
     + cl/typecase.go
     - pkg/cl/unless.go
     - pkg/cl/unwind-protect.go
     - pkg/cl/util.go
     - pkg/cl/when.go
     - pkg/cl/with-input-from-string.go
     - pkg/cl/with-open-file.go
     - pkg/cl/with-open-stream.go
     - pkg/cl/with-output-to-string.go
     - pkg/cl/with-standard-io-syntax.go
     - pkg/clos/with-slots.go
     - pkg/gi/dovector.go
     - pkg/gi/recover.go
     - pkg/gi/select.go
     - pkg/gi/with-input-from-octets.go
     - pkg/gi/with-mutex-lock.go
     - pkg/gi/with-zip-reader.go
     - pkg/gi/with-zip-writer.go
     - pkg/test/assert-panic.go
     - pkg/test/bind.go
     - pkg/test/test.go


  - more complex tests
  - should there be tests for cover.lisp somewhere?
   - lisp/test
    - testdata for files
    - call go to execute tests with explanation that want to use the latest and for CI
     - use slipr along with Makefile

 - remove covx.lisp and testx.lisp after testing

 - later could be interactive
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

- allow package to call in package functions without export

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
