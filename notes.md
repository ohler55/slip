# SLIP Notes

- **multipass** or utm for linux

- next

 - datagram branch (https://www.sbcl.org/manual/#Networking)
  - socket-receive (datagram)
   - should oob option be removed?
  - socket-send (datagram)
  - different functions for send and receive of datagrams with oob
   - socket-receive-message (args same as socket-receive but with oob)
    - oob should be buf or nil, a key value :oob if provided implies oob flag
   - socket-send-message ()

  - should different functions be used since there is more info returned?

 - cl-output branch
  - finish-output
  - force-output
  - clear-output

 - name service branch
  - host-ent class
  - get-protocol-by-name (name) => fixnum, name, aliases
  - get-host-by-name
  - get-host-by-address


  - classes (all socket flavors add no additional methods) (are they really needed?)
   - inet-socket
   - inet6-socket
   - local-socket
   - local-abstract-socket


 - slot-value
  - add slot-value to flavors pkg/clos (for get and setf)
   - always allow


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

 - misc
 * [ ] CASE (not ccase)
 * [ ] ECASE
 * [ ] REDUCE
 * [ ] NRECONC
 * [ ] REVAPPEND
  - structs?

 - clos methods/generics (flavors and clos mix as flos)
  - flos
   - defgeneric is only used to check defmethod
   - inherit flos-flavor to generate functions for all method with a designated prefix
  - is a standard-class needed instead of vanilla or maybe just expand vanilla?

  - change-class (for flavors instances only for now)
   - parts of instance interface? same as class-of

 - merge (starts with result-type)

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
