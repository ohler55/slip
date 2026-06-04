# SLIP Notes

- **multipass** or utm for linux

---------------------

- add provenance
 - with flag to slip, slap, and slapper
  - keep file content in memory by full path
  - in code read keep track of file, line, and column for each function id (address)
   - form map or sorted slice with key of pointer to lists
   - a second map of func pointer and location info
    - set in CompileList if provenance flag is true
 - how to handle multiple files packed into one
  - maybe place a comment directive in front of file
   - also works for compiling buffers
 - maybe change starts to be location info and stack index
  - or maybe take address of first element - does that stay the same when list is formed?
   - doesn't work for nested lists
  - when list is formed add info to list map
 - which way, extend functions or separate map
  - extend function
   - more directly accessible
   - always available
   - simpler, maybe
  - separate
   - less memory if not in use
   - can't turn it on midway through, has to be on start

 - use for stack trace
 - use for coverage

 - plan
  - provenance.go
   - Prov struct
    - filepath string
    - firstLine uint32
    - lastLine uint32
    - firstColumn uint16
    - lastColumn uint16
   - flag for provenance on/off
    - SetProvenance(on bool)
    - just store original file contents (is that even needed?)
     - if separate prov info then also add to map
     - if in func then nothing else to do
   - for tracing, maybe just add filename:line:col as a prefix and keep current the same otherwise
    - for non-file loaded skip that part (just use a few spaces for indent)
    - maybe just filepath base without .lisp then firstLine and firstColumn

  - CompileList should lookup list ptr and set new func ptr or populate func prov
   - pass in list ptr map/sorted-slice
  - code.closeList add to map in prov info
  - use (declaim (optimize (filepath foo.lisp)) to switch file in all-at-once loading in app

  - net:available-port ()

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
