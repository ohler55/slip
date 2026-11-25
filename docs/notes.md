# SLIP Notes

- **multipass** or utm for linux

---------------------

- replace ReadProv in slap
- replace CompileWithProvenance in slap

---------------------
- add a fixnum sorted collection - fixnum-table fixnum-set fixnum-dict Positive Indexed Sorted Set (PISS)
 - fast adds
 - fast lookups
 - burn code or old provenance
 - both function and flavor-like?
 - functions
  - piss-clear (pis)
  - piss-get (pis index)
   - also a placer for setf
  - piss-has (pis index)
  - piss-set (pis index value)
  - piss-rem (pis index)
  - make-piss ()
  - pissp (obj)
  - piss-count (pis)
  - mappiss (function piss) - function take 2 args, index and value
  - with-piss-iterator (next piss) *forms
   - next is a function that return a values of 3; has-next, index, value
  - piss-add (pis &rest index-values)
   - index-values can be assoc or property list, detect by first value or allow a mix?
   - more efficient way to bulk add data

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
