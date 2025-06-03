# SLIP Notes

- **multipass** or utm for linux

- next

 - pretty-print
  + Let (let and let*)
  + Defun (defun defmacro)
  + Lambda
  + Defvar
  + Fun1i2
  - defflavor
    defflavor foo (x
                   y)
                  (bar)
      :inittable-instance-variables)
   - tighter by keep flavorname on first line
   - really tight then 4 indent for flavor and vars, other 2
  - defmethod, defwhopper - first two, method spec and args are indent 4 if newline

  - symbol lookup
   - flavor - deflavor
    - creates a 3 way import loop
    - add func to slip.Class interface
     - DefList() List ???
     - DefMethodList
      - of DefMethod => FuncInfo?, pack flavor, daemon, and name into fi.Name
      - or **list**
     - AllMethods list of method lists
     - add gi functions to return the same

   - flavor:method or flavor:daemon:method - defmethod or defwhopper
   - instance
    - maybe (make-instance 'flavor :x 1 :y 2)
     - use (setf (slot-value inst x) 1)
      - for all, make-instance naked
     - wrap the make-instance with a let and then the set calls and comments

   - package:symbol or package::symbol
    - if not func and not var and contains : or . then flavor method or package symbol

  - Flavor
  - if symbol lookup what it is bound to
   - func
   - flavor
   - method? how to designate in a word vanilla.describe or something like that
   - var - (defvar xx ...)
  - use in cl:disassemble


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
