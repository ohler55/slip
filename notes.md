# SLIP Notes

- **multipass** or utm for linux

- next

 - standard-flavor branch
  - functions
   - [ ] CHANGE-CLASS
    - test
   - [ ] WITH-SLOTS

   - [x] CLASS-NAME
   - [x] CLASS-OF
   - [x] DEFCLASS - needs accessors
   - [x] DEFINE-CONDITION
   - [x] DOCUMENTATION
   - [x] FIND-CLASS
   - [x] INVALID-METHOD-ERROR
   - [x] MAKE-INSTANCE
   - [x] print-not-readable-object
   - [x] print-not-readable
   - [x] class-precedence
   - [x] class-supers
   - [x] class-metaclass
   - [x] list-all-classes
   - [x] SLOT-BOUNDP
   - [x] SLOT-EXISTS-P
   - [x] SLOT-MAKUNBOUND
   - [x] SLOT-MISSING
   - [x] SLOT-UNBOUND
   - [x] SLOT-VALUE

---------------------
 - bugs
  - open doesn't handle absolute paths nor ~
   - ~someone/x
---------------------

 - generics branch
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

  - generics are tied to a package just like functions
  - is a flag needed to indicate some generics/method do not allow qualifiers like :before and :after
  - sparse method combinations, no need for empties
  - add flavor/class hierarchy to regular hierarchy
   - build class then instance get hierarchy from class/flavor
    - maybe a name class hierarchy as well as instance
    - precedence-list
     - foo super-foo standard-object t

  - clos https://lispcookbook.github.io/cl-cookbook/clos.html and https://www.algo.be/cl/documents/clos-guide.html
   - clos.Generic
    - start with built in types
    - embeds flavors.Method
    - adds parameters - list of type symbols, later type specifier lists
    - add specifics []*clos.Generic
   - want this on package for lookup
   - clos instances don't bind slot by default
    - not a slip.Scope, just map or rather a specific Slots map so cl/slot-value works with it
     - Getter interface or Getter and Setter interfaces
  - call-next-method, alias for continue-whopper

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

 - flavor allow out of order defflavor like standard-class
 - rename bag-flavor to just bag?

 - bonus functions

 - [ ] SLOT-MISSING - make generic
 - [ ] SLOT-UNBOUND - make generic
 - [ ] ADD-METHOD
 - [ ] CALL-METHOD
 - [ ] CALL-NEXT-METHOD
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
