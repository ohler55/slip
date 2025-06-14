# SLIP Notes

- **multipass** or utm for linux

- next

 - encrypted app
  - create make-app
   - options to set fields in a slip.App
   - write and build
  - test
   - AppArg with a flag.FlagSet and then look at with Visit or VisitAll
   - update scope with new scope
   - DefaultReadable with nil and other expected types

  - slip.Class interface
   - AllMethods() slip.List

  - symbol lookup
   - AllMethods list of method lists or should it be a list of names only?
    - AllDefMethods
    - add gi functions to return the same

   - instance
    - maybe (make-instance 'flavor :x 1 :y 2)
     - use (setf (slot-value inst x) 1)
      - for all, make-instance naked
     - wrap the make-instance with a let and then the set calls and comments

  - use in cl:disassemble for all symbols like pp.Append


 - save-state (destination &key order) (snapshot)
  - set current package
  - keep track of require imports
   - pkg/cl/require should store order of call, name, and path of loaded
  - state file is just lisp
   - requires
   - variable setq (use correct package)
    - check boundp first then defvar or setq
    - to preserve reference to the same object form a temporary map, makunbound after
   - defuns for all lambda function like dissasemble (use correct package)
  - order (sort) option
   - require and vars always alphabetical
   - functions alpha, dependency, reverse-dependency


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

 - better-methods
  - instance method should use flavor so defmethod give existing instances new methods
  - could create a struct for the methods slice to avoid a lookup on recieve


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
