# SLIP Notes

- **multipass** or utm for linux

- next

 - misc
  - [x] CONSTANTLY
  - [x] DECODE-UNIVERSAL-TIME
  - [x] DRIBBLE
  - [x] ELT
  - [x] ENCODE-UNIVERSAL-TIME
  - [x] ERROR
  - [x] END-OF-FILE
  - [x] ENDP
  - [x] FILL
  - [x] GET-DECODED-TIME
  - [x] GET-UNIVERSAL-TIME
  - [x] MAKE-SEQUENCE
  - [x] MAP-INTO
  - [x] MISMATCH
  - [x] MULTIPLE-VALUE-BIND
  - [ ] MULTIPLE-VALUE-CALL
  - [ ] MULTIPLE-VALUE-LIST
  - [ ] MULTIPLE-VALUE-PROG1
  - [ ] MULTIPLE-VALUE-SETQ
  - [ ] MULTIPLE-VALUES-LIMIT
  - [ ] REPLACE
  - [ ] ROTATEF
  - [ ] SHIFTF
  - [ ] SPECIAL-OPERATOR-P
  - [ ] SUBTYPEP
  - [ ] TRUENAME
  - [ ] UNWIND-PROTECT
  - [ ] USER-HOMEDIR-PATHNAME
  - [ ] WILD-PATHNAME-P
  - [ ] WITH-STANDARD-IO-SYNTAX
  - [ ] Y-OR-N-P
  - [ ] YES-OR-NO-P
  - gi:time-to-universal-time
  - gi:universal-time-to-time
  - [ ] *TERMINAL-IO* (stream that uses *standard-input* and *standard-output* or maybe stdin and stdout always)
   - use cl.TwoWayStream

 - math
  - [ ] ACONS
  - [ ] ACOSH
  - [ ] ASINH
  - [ ] ATANH
  - [ ] BIGNUM
  - [ ] CIS
  - [ ] COSH
  - [ ] DECODE-FLOAT
  - [ ] DENOMINATOR
  - [ ] DIVISION-BY-ZERO
  - [ ] FLOAT-DIGITS
  - [ ] FLOAT-PRECISION
  - [ ] FLOAT-RADIX
  - [ ] FLOAT-SIGN
  - [ ] GCD
  - [ ] IMAGPART
  - [ ] INTEGER-DECODE-FLOAT
  - [ ] INTEGER-LENGTH
  - [ ] ISQRT
  - [ ] LCM
  - [ ] LDB
  - [ ] LDB-TEST
  - [ ] NUMERATOR
  - [ ] PHASE
  - [ ] REALPART
  - [ ] SCALE-FLOAT
  - [ ] SINH
  - [ ] TANH

 - bits-and-bytes
  - [ ] BIT
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
  - [ ] BIT-VECTOR-P
  - [ ] BIT-XOR
  - [ ] BYTE
  - [ ] BYTE-POSITION
  - [ ] BYTE-SIZE
  - [ ] DEPOSIT-FIELD
  - [ ] DPB
  - [ ] MASK-FIELD
  - [ ] SBIT
  - [ ] SIMPLE-BIT-VECTOR
  - [ ] SIMPLE-BIT-VECTOR-P

 - tough-ones
  - [ ] DESTRUCTURING-BIND
  - [ ] FORMATTER


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
