# SLIP Notes

- **multipass** or utm for linux

- next

---------------------
 - GoMakeOnly in flavors should be checked in clos.MakeInstance or pass a flag indicating go or lisp

 - generics branch
  - handle all type in make-load-form

  - DefLister
   - standard-object
    - condition should call make-condition
   - flavors.Instance

  - clos https://lispcookbook.github.io/cl-cookbook/clos.html and https://www.algo.be/cl/documents/clos-guide.html

 - [x] SLOT-MISSING
 - [x] SLOT-UNBOUND - make generic
 - [x] CALL-NEXT-METHOD
 - [x] COMPUTE-APPLICABLE-METHODS
 - [x] DEFGENERIC
 - [x] DEFMETHOD - need generic support
 - [ ] ENSURE-GENERIC-FUNCTION
 - [x] FDEFINITION
 - [x] FIND-METHOD
 - [ ] FUNCTION-KEYWORDS
 - [x] GENERIC-FUNCTION
 - [ ] INITIALIZE-INSTANCE
 - [ ] MAKE-LOAD-FORM - same as what is called by snapshot
       - test with instance as well (also flavor instance)
       - class
       - condition
       - generic
       - ordinary function?
        - might need fdefinition to test
 - [ ] MAKE-LOAD-FORM-SAVING-SLOTS
 - [ ] METHOD-COMBINATION-ERROR
 - [x] METHOD-QUALIFIERS
 - [ ] NEXT-METHOD-P
 - [x] NO-APPLICABLE-METHOD
 - [ ] NO-NEXT-METHOD - generic that defaults to a panic
 - [ ] REINITIALIZE-INSTANCE
 - [ ] REMOVE-METHOD
 - [ ] SHARED-INITIALIZE
 - [ ] SYMBOL-MACROLET
 - [ ] UPDATE-INSTANCE-FOR-REDEFINED-CLASS
 - [ ] WITH-ACCESSORS
 - [ ] WITH-ADDED-METHODS
 - [ ] CHANGE-CLASS - call generic update-instance-for-different-class

---------------------
 - flavor allow out of order defflavor like standard-class
 - rename bag-flavor to just bag?

---------------------
- docs to explain implementation and deviations of clos and others

-----------------
  - [ ] inspect [interactive]

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


 - structs - seems like a downgrade from class or flavors, just another weaker alternative instances with slots

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


 - clos methods/generics (flavors and clos mix as flos)
  - flos
   - defgeneric is only used to check defmethod
   - inherit flos-flavor to generate functions for all method with a designated prefix
  - is a standard-class needed instead of vanilla or maybe just expand vanilla?

  - change-class (for flavors instances only for now)
   - parts of instance interface? same as class-of

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

 - other method combinations?
  - :method-combination option for defflavor
  - daemon (default)
  - progn
  - or
  - and
  - list

- ui with fyne
