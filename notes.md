# SLIP Notes

- **multipass** or utm for linux

- next

 - snapshot (destination)

  - order in file
   - comment with date saved
   - requires
   - defpackage excluding existing
   - functions for each package, usually user
   - flavors
   - constants
   - vars for each package, just use user if var is in user
    - defvar then setq
  - try loading in sbcl without requires

  - for vars, skip ones that can't be set such as streams, channels, etc
   - is readable based on a map of types?

  - set current package before defxxx

  - state file is just lisp
   - requires
   - variable setq (use correct package)
    - check boundp first then defvar or setq
    - to preserve reference to the same object form a temporary map, makunbound after
   - defuns for all lambda function like dissasemble (use correct package)
  - instance
   - maybe (make-instance 'flavor :x 1 :y 2)
    - use (setf (slot-value inst x) 1)
     - for all, make-instance naked
    - wrap the make-instance with a let and then the set calls and comments


 - clos https://lispcookbook.github.io/cl-cookbook/clos.html and https://www.algo.be/cl/documents/clos-guide.html
  - defclass
  - defmethod class daemon bindings/args
   - find class to determine how to define
   - maybe separate function/method table for all clos classes
    - when executing consider based method-qualifier
    - map of function name and methods
     - method includes:
      - functions to call
       - daemons
       - most specific type for primary
      - qualifier
       - list of var-name and type pairs
        - match is when all types for the arguments match
        - nill type matches anything
       - if no qualifier then it is generic
      - docs
  - defgeneric
  - optimize for binding of one to a class using the class method
  - register as normal function but handle differently
  - move defmethod to clos

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
