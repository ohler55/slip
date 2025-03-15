# SLIP Notes

- **multipass** or utm for linux

- next

 - bits-and-bytes
  - [ ] BIT - new built in type as byte that is 0 or one
  - [ ] BIT-AND
  - [ ] BIT-ANDC1
  - [ ] BIT-ANDC2
  - [ ] BIT-EQV
  - [ ] BIT-IOR
  - [ ] BIT-NAND
  - [ ] BIT-NOR
  - [ ] BIT-NOT
  - [ ] BIT-ORC1
  - [ ] BIT-ORC2
  - [ ] BIT-VECTOR
   - like octets
    - encoding/asn1/BitString ?not that helpful? maybe just as a model for impl
   - operator on a bit-vector
   - display as 0 and 1 or #*010101
   - update code to read
   - coerce to list, maybe other for non-standard like integer, octet, byte
   - maybe always a simple-bit-vector, not displaced
  - [ ] BIT-VECTOR-P
  - [ ] BIT-XOR
  - [x] BYTE (function and also type)
   - type is ok as octet
   - function return a byte-specifier
   - need byte-specifier
   - maybe change to arbitry bits with some size indicator

  - [ ] BYTE-POSITION
  - [ ] BYTE-SIZE
  - [ ] DEPOSIT-FIELD
  - [ ] DPB
  - [ ] LDB
  - [ ] LDB-TEST
  - [ ] MASK-FIELD
  - [ ] SBIT
  - [ ] SIMPLE-BIT-VECTOR
  - [ ] SIMPLE-BIT-VECTOR-P

 - tough-ones
  - [ ] DESTRUCTURING-BIND
  - [ ] FORMATTER
  - [ ] INTEGER-DECODE-FLOAT


- autocomplete
 - for describe and others>

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
