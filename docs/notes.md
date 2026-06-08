# SLIP Notes

- **multipass** or utm for linux

---------------------

- add provenance
 - documentation function should replace _ and __ as needed and wrap lines
 - lisp functions
  - reset-coverage (hard)
  - write-coverage (filename &optional destination)
   - nil should return string mostly for testing or for progress analysis
 - test with defun
 - what to do with repl defined functions?
  - maybe keep counter of editor.evalForm() calls then REPL-<counter> as filename


 - use for stack trace
  - for tracing, maybe just add filename:line:col as a prefix and keep current the same otherwise
   - for non-file loaded skip that part (just use a few spaces for indent)
   - maybe just filepath base without .lisp then firstLine and firstColumn
   - options for error stack as globals?
    - *stack-trace-format*
     - :provenance (default)
      - provenance if available and function otherwise
     - :provenance-long (full file path)
     - :function
     - :both
     - :both-long
   - in AppendFull
 -

 - coverage app
  - start with writing colorized file(s)
   - lisp code
   - where to put the code?
    - lisp dir in slip?
  - later could be interactive
  - lisp app for processing and coverage
  - add file checksum function to support lisp coverage
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
