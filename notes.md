# SLIP Notes

- **multipass** or utm for linux

- next

---------------------
 - GoMakeOnly in flavors should be checked in clos.MakeInstance or pass a flag indicating go or lisp


 - generics branch
  - test defgeneric, defmethod, and method combinations

  - make sure snapshot works as expected
  - standard-class reader, writer, and accessor
   - class slots
    - accessors must consider slot on standard-object.Type.Vars
     - if not found then look back on inherit
  - pkg/gen or pkg/generic
   - might need to move defmethod (defmethod depends on flavors)
   - maybe add VarName() to slip.Class to break dependency

  - call print-object in slip.Panic to form message
   - if condition and :report then use that for printing, others like *print-escape*
    - else call generic
    - if no generic match print .String() - should be generic method for t
  - pass scope in then various error creation panic
   - allow for report function to use local variables

  - clos https://lispcookbook.github.io/cl-cookbook/clos.html and https://www.algo.be/cl/documents/clos-guide.html

 - [ ] SLOT-MISSING - make generic
 - [ ] SLOT-UNBOUND - make generic
 - [ ] ADD-METHOD
 - [ ] CALL-METHOD
 - [ ] CALL-NEXT-METHOD
 - [ ] CHANGE-CLASS - call generic update-instance-for-different-class
 - [ ] COMPUTE-APPLICABLE-METHODS
 - [ ] DEFGENERIC
 - [ ] DEFINE-METHOD-COMBINATION
 - [ ] DEFMETHOD - need generic support
 - [ ] DESCRIBE-OBJECT
 - [ ] ENSURE-GENERIC-FUNCTION
 - [ ] FIND-METHOD
 - [ ] FUNCTION-KEYWORDS
 - [ ] GENERIC-FLET
 - [ ] GENERIC-FUNCTION
 - [ ] GENERIC-LABELS
 - [ ] INITIALIZE-INSTANCE
 - [ ] MAKE-LOAD-FORM
 - [ ] MAKE-LOAD-FORM-SAVING-SLOTS
 - [ ] MAKE-METHOD
 - [ ] METHOD-COMBINATION
 - [ ] METHOD-COMBINATION-ERROR
 - [ ] METHOD-QUALIFIERS
 - [ ] NEXT-METHOD-P
 - [ ] NO-APPLICABLE-METHOD
 - [ ] NO-NEXT-METHOD
 - [ ] PRINT-OBJECT
 - [ ] REINITIALIZE-INSTANCE
 - [ ] REMOVE-METHOD
 - [ ] SHARED-INITIALIZE
 - [ ] SYMBOL-MACROLET
 - [ ] UPDATE-INSTANCE-FOR-REDEFINED-CLASS
 - [ ] WITH-ACCESSORS
 - [ ] WITH-ADDED-METHODS

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
