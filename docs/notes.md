# SLIP Notes

- **multipass** or utm for linux

---------------------

- app-support
 + gi:*app-args*
 + process flavor
 + find-process (pid)
 - command flavor

- system
 - make sure require can load a system
 - add load-system (system-designator)
  - :quux or 'quux or "quux"
 - system files be .asd
  - one defsystem in file, read, compile, eval to get system
  - tell system fetch and load
 - use load-system to read in file, eval, and then call fetch and load

 - command (like net:request, use os/exec)
  - make-instance or make-command
  - :run (&key :stdout <boolean> :stderr <boolean> :wait <boolean>)
  - settable before :run and read only after that
   - :stdin input-stream
   - :stdout output-stream
   - :stderr output-stream
   - :env
   - :path
   - :args
   - :dir
   - :process (read only)

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
