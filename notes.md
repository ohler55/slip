# SLIP Notes

- **multipass** or utm for linux

- next

---------------------
 - GoMakeOnly in flavors should be checked in clos.MakeInstance or pass a flag indicating go or lisp

 - plugins
  - PkgUse with clos and generics

- generics branch
 - replace all in slip and plugins
  - NewPanic => ErrorPanic where there is a scope and depth (only in slip)
  - PanicMethodArgChoice => MethodArgChoicePanic (slip and slip-flow)

----------
- for later
 - [ ] WITH-ACCESSORS
 - [ ] ENSURE-GENERIC-FUNCTION
 - [ ] REINITIALIZE-INSTANCE
 - [ ] UPDATE-INSTANCE-FOR-REDEFINED-CLASS
 - [ ] CHANGE-CLASS - call generic update-instance-for-different-class

  - clos https://lispcookbook.github.io/cl-cookbook/clos.html and https://www.algo.be/cl/documents/clos-guide.html

-------------------
 - readably branch
  - add Readably or **Readable** interface
   - Readably(b []byte, op ...*slip.Printer) []byte
    - always readable but op[0] for base, radix, prec
    - maybe op[0] for pretty and rightMargin
    - maybe op[0].Case
   - use in printer, pp, snapshot, etc can then decide

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
