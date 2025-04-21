# SLIP Notes

- **multipass** or utm for linux

- next

 - bits-and-bytes
  - [x] BIT
  - [x] BIT-AND
  - [x] BIT-ANDC1
  - [x] BIT-ANDC2
  - [x] BIT-EQV
  - [x] BIT-IOR
  - [x] BIT-NAND
  - [x] BIT-NOR
  - [x] BIT-NOT
  - [x] BIT-ORC1
  - [x] BIT-ORC2
  - [x] BIT-VECTOR
  - [x] BIT-VECTOR-P
  - [x] BIT-XOR
  - [x] BYTE
  - [x] BYTE-POSITION
  - [x] BYTE-SIZE
  - [x] DEPOSIT-FIELD
  - [x] DPB
  - [ ] LDB
  - [ ] LDB-TEST
  - [ ] MASK-FIELD
  - [ ] SBIT
  - [x] SIMPLE-BIT-VECTOR
  - [x] SIMPLE-BIT-VECTOR-P
   - update simple-vector-p

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
