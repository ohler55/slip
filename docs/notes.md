# SLIP Notes

- **multipass** or utm for linux

---------------------

- expand-system
 - system (not ASDF but similar) https://asdf.common-lisp.dev/asdf.html
  - add :pathname to system for the default location of component files
  - components
   - allow filename of (:file "filename")
    - other keywords in (:file "quux" :pathname "src-dir" :description "has random stuff in it")
     - :description string
     - :pathname pathname-specifier
    - if (:file quux) is used then file should be the file name only and :pathname indicates the dir

 - require and load can not easily be made to do the same as load-system dues to circulat dependency
  - load alway create some kind of interface but maybe best to leave them as just loading the def and not fetch and load
 - load-system (system)
  - don't bother with &rest keys &key force force-not verbose version &allow-other-keys

  - :quux or 'quux or "quux" for system
  - set *load-pathname* and *load-truename* during load and eval
  - defer package back to before loading
 - system files can/should be .asd
  - one defsystem in file, read, compile, eval to get system
   - other code before defsystem will not break
  - tell system fetch and load
 - use load-system to read in file, eval, and then call fetch and load
  - same as require and load but must receive system
   - must contain only a defsystem or could be more forgiving

- :required-methods is broken
 - should not check for required on defflavor as methods are defined later
  - maybe check on first make-instance?


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
