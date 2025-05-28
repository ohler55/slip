# SLIP Notes

- **multipass** or utm for linux

- next

 - pretty-print
  - algo fails by not giving more priority to children
   - maybe squeeze then let children be less tight
   - or start with usual and tighten until or
    - if no success then squueze
   - for a list
    - try usual
    - try tight if all children are usual
    - squeeze otherwise
   - maybe calc min and max widths
   - maybe use min left for children
    - then parent decides on layout given it's max width
   - think about how I format code
    - layout as usual then line breaks, how are those determined
    - or focus on lines after first layout?
     - maybe generate with usual then look for long lines
      - for a long line look for place to break
      - will need to look up a few lines as well
      - line would be not only text but also an overlay with references to slip objects
      - gets a little too complicated
   - each node
    - has left and right [and width]
    - has line number?
    - isTight, isSqueezed
    - steps
     - optimal or usual layout first
     - refine layout
      - look for longer than max
       - if found then back to top most less tight one


   - pLet (let and let*)
   - pDefun (defun defmacro)
    - wrap doc strings
   - pLambda
   - pCond
   - pDefvar
   - pLoop (dotimes?)
   - pWith (with-output-to-string, other with-xxx)
   - pFlavor
   - if symbol lookup what it is bound to
    - func
    - flavor
    - method? how to designate in a word vanilla.describe or something like that
    - var - (defvar xx ...)


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
