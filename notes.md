# SLIP Notes

- **multipass** or utm for linux

- next

 - multi-stream
  - interfaces
   - LastBytePeeker

  - [ ] OPEN-STREAM-P
  - [ ] STREAM-ELEMENT-TYPE - always return octet
  - [ ] STREAM-EXTERNAL-FORMAT - :default
  - [ ] FRESH-LINE
   - same as terpri unless a file or string then check last byte
   - use interface for last byte written LastBytePeeker { LastByte() }

  - update (maybe with interfaces)
   - file-length
    - use HasLength interface
   - file-posiion
    - use HasPosition interface

  - [ ] BROADCAST-STREAM
   - define at top level or cl?
   - slice of output streams that are io.Writers, check on make
   - streamp => t
   - output-stream-p => t
   - open-stream-p
    - test for broadcast stream open, not streams
     - maybe open if more than zero streams??
   - stream-element-type
   - fresh-line
   - input-stream-p => nil
   - close - maybe zero out streams
    - must be io.Closer
   - file-length
   - file-posiion
   - stream-external-format

  - [ ] MAKE-BROADCAST-STREAM
  - [ ] BROADCAST-STREAM-STREAMS

  - [ ] CONCATENATED-STREAM
  - [ ] MAKE-CONCATENATED-STREAM
  - [ ] CONCATENATED-STREAM-STREAMS

  - [ ] ECHO-STREAM
  - [ ] MAKE-ECHO-STREAM
  - [ ] ECHO-STREAM-INPUT-STREAM
  - [ ] ECHO-STREAM-OUTPUT-STREAM

  - [ ] TWO-WAY-STREAM
  - [ ] MAKE-TWO-WAY-STREAM
  - [ ] TWO-WAY-STREAM-INPUT-STREAM
  - [ ] TWO-WAY-STREAM-OUTPUT-STREAM

  - [ ] OPEN-STREAM-P
  - [ ] STREAM-ELEMENT-TYPE
  - [ ] STREAM-EXTERNAL-FORMAT
  - [ ] INTERACTIVE-STREAM-P
  - [ ] FRESH-LINE
  - [ ] WITH-STANDARD-IO-SYNTAX

 - case
  - [ ] CTYPECASE
  - [ ] TYPECASE
  - [ ] ETYPECASE
  - [ ] OTHERWISE

 - misc
  - [ ] CONSTANTLY
  - [ ] DECODE-UNIVERSAL-TIME
  - [ ] DESTRUCTURING-BIND
  - [ ] DRIBBLE
  - [ ] ELT
  - [ ] ENCODE-UNIVERSAL-TIME
  - [ ] ERROR (function)
  - [ ] END-OF-FILE
  - [ ] ENDP
  - [ ] FILL
  - [ ] FORMATTER
  - [ ] GENTEMP
  - [ ] GET-DECODED-TIME
  - [ ] GET-UNIVERSAL-TIME
  - [ ] MAKE-SEQUENCE
  - [ ] MAP-INTO
  - [ ] MISMATCH
  - [ ] MULTIPLE-VALUE-BIND
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
  - [ ] Y-OR-N-P
  - [ ] YES-OR-NO-P

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
