# SLIP Notes

- **multipass** or utm for linux

- next

 - pretty-print and *print-pretty*
  - add function to gi
   - use *print-right-margin*
   - force *print-base* *print-case* *print-escape* *print-level* *print-lines* *print-readably* *print-radix* *print-array*
  - better algorithm for specific first words in list (defun let let* cond progn and others)
   - Printer createTree and appendTree improvement
   - create tree with values or children set
    - use print vars to create leaf strings
    - can each node implement a common interface for ideal, tight, and squeeze?
     - leaf and branch nodes, different branch node type for defun, let, quote, cond, etc
    - maybe a tightness args passed down and decremented each time
     - greater than 0 then tighter
     - if less than zero then squeeze
     - when 0 no need to decrement, use ideal
  - rules
   - defun and defmacro
   - let and let*
   - cond
   - defvar
   - loop
  - build tree with min, max, and preferred width
   - node
    - name
    - left
    - width
    - layout - match funcs
    - children
    - layout funcs
     - ideal(left int) (right int) or maybe
     - tight
     - squeeze
   - algorithm
    - start with preferred
     - calculate ideal, tight, and squuze widths
     - parent then decides which layout to use and calls to set left
      - left of 0 indicates same line
    - try to fit by adjust at higher levels first
    - if needed go to more extremes (example is (list
                                                 one))

 - save-state (destination &key order)
  - set current package
  - keep track of require imports
   - pkg/cl/require should store order of call, name, and path of loaded
  - state file is just lisp
   - requires
   - variable setq (use correct package)
    - check boundp first then defvar or setq
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
