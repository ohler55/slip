# SLIP Notes

- **multipass** or utm for linux

---------------------

- sequence-io
 + copy-stream-to-stream (input output &key buffer-size)
 - read-sequence (sequence stream &key start end)
  - read into octets (maybe limit to octets)
 - write-sequence
  - update to also support octets

---------------------

- clipboard-extras
  - add C-Y with pbpaste
  - C-k should add to clipboard
  - M-d should copy to clipboard
  - M-DEL should copy to clipboard

  - test osc52 with iterm2

---------------------

- rethink read/compile/eval cycle
 - try and remove special case for defxxx functions and in-package
 - maybe postpone as much as possible, keep psuedo functions (undefined) around but have them be able to resolve
 - how about first pass, just lists
  - second pass convert to functions (maybe combine with first)
  - third call each including defuns
   - calls need to resolve functions in this step
 - plan
  - try not having compile for some test code
  - once that works, make compile do nothing
  - finally no special case for defxxx


---------------------

- make coverage interactive
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

- support mouse in repl
 - for what purpose?
 - https://iterm2.com/feature-reporting/

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
