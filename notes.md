# SLIP Notes

- **multipass** or utm for linux

- next

 - pp should also set *print-escape* to true

 - clos https://lispcookbook.github.io/cl-cookbook/clos.html and https://www.algo.be/cl/documents/clos-guide.html
  - redefine flavors.Method
   - update docs on other flavors
    - watch
     - client
     - channeler
     - printer
     - server
    - test
     - suite
     - test
     - testable
   - fix clos test TestMethodDocFromFunc

   - test multiple inheritance
   - test inherit after defflavor
    - make sure vanilla is at end
     - remove and then add back if present?
   - slip.Method
    - change flavors to use Method
     - maybe start with whoploc
     - defmethod
     - instance receive
     - flavor
     - defwhopper

    - validate parameters on defmethod to make sure the funcdocs are the same
   - does Method need to be at top level
    - needed for Generic which will be used by Package

   - clos.Generic
    - embeds flavors.Method
    - adds parameters - list of type symbols, later type specifier lists
    - add specifics []*clos.Generic
   - want this on package for lookup

  - defgeneric (needed before defclass for slot accessors)
   - build generic dispatch table on Package
   - as default and flavor instance (send inst function-name args) if it has :function-name
   - map by name to *Generic
   - Generic [same as clos Method conceptually]
    - name
    - methods (lookup based on arg types)
     - branch for each specializer (arg types)
      - nested maps or a list?
       - if map
        - need to consider sub-classes so not a direct lookup
       - if list then walk and check each arg-type until a match
        - sort order is most specialize
        - maybe more of a tree with first level as first arg then branches off for 2 and additions arguments
     - leaf has daemons like flavors - can tht be reused?
     - as method defined the qualifiers/daemons are propogated to reduce call time lookups
    - Method or Generic to avoid name confusion
     - embed flavors.Method
     - parameters - list of arg types - just types, no forms, or maybe rely on coerce type matching
     - specifics - []*Method to search


  - defclass (class-name superclass-names slot-specifiers &rest class-options*)
   - update clos/class struct
   - implement for vars/slots only
   - class-options
    - :default-initargs init-arg-list
    - :documentation string
    - :metaclass classname - limit to standard-class and ignore otherwise
   - slot-specifier like (name :initarg :name :accessor name)
    - :reader reader-function-name * - names of functions to access slot
    - :writer writer-function-name *
    - :accessor reader-function-name *
    - :allocation allocation-type
    - :initarg initarg-name *
    - :initform form
    - :type type-specifier
    - :documentation string
   - make sure all validation is handled for name collisions
  - standard-class
  - standard-object - like vanilla

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
