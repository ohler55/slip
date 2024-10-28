# SLIP Notes

- **multipass** or utm for linux

- next

 - misc
 * [ ] CASE (macro)
  - case (keyform &rest clause)
   - keyform is evaluated
   - clauses are not
    - car of clause is compared to key if not a list or to list members if a list
    - if car is otherwise or t then eval that as return value
     - if otherwise is not last then fail on eval with bad case clause
 * [ ] ECASE
 * [ ] MAP
 * [ ] MERGE
 * [ ] NRECONC
 * [ ] PROGV
 * [ ] REDUCE
 * [ ] REVAPPEND
 * [ ] SLOT-BOUNDP
 * [ ] SLOT-EXISTS-P
 * [ ] SLOT-MAKUNBOUND
 * [ ] SLOT-MISSING
 * [ ] SLOT-UNBOUND
 * [ ] SLOT-VALUE
  - add slot-value to flavors pkg/clos (for get and setf)
   - always allow
 * [ ] THE
 * [ ] WITH-INPUT-FROM-STRING
 * [ ] WITH-OPEN-STREAM
 * [ ] WITH-OUTPUT-TO-STRING

 - jetstream - update with latests model
  - can that be made to work with nats or apphub?

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
