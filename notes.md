# SLIP Notes

- **multipass** or utm for linux

- next

 - pivot to socket (https://www.sbcl.org/manual/#Networking)

  - rename the peer and local functions

  - functions
   + socket-p - not in sbcl
   + make-socket - not in sbcl
   + socket-pair - not in sbcl
   + socket-close
   + socket-state - not in sbcl
   + socket-select - not in sbcl
   + wait-for-input - not in sbcl
   + parse-address - not in sbcl
   + socket-stream - not in sbcl
   + socket-open-p
   + socket-name
   + socket-address - not in sbcl
   + socket-port - not in sbcl
   + socket-peername
   + socket-peer-address - not in sbcl
   + socket-peer-port - not in sbcl
   - socket-receive (datagram)
   - socket-send (datagram)
   + socket-option
   + socketopt-reuse-address
   - socketopt-keep-alive
   - socketopt-oob-inline
   - socketopt-debug
   - socketopt-broadcast
   - socketopt-tcp-nodelay

   - socket-make-stream
    - use sockopts for timeouts and others
   - socket-accept
   - socket-bind
   - socket-connect
   - socket-listen
   - socket-shutdown - use syscall.Shutdown with how of SHUT_RD, SHUT_WR, or SHUT_RDWR
   - non-blocking-mode
   - make-inet-address (string) => octets
   - make-inet6-address (string) => octets
   - get-protocol-by-name (name) => fixnum, name, aliases
   - get-host-by-name
   - get-host-by-address
  - classes (all socket flavors add no additional methods)
   - inet-socket
   - inet6-socket
   - local-socket
   - local-abstract-socket
   - host-ent
  - cl
   - finish-output
   - force-output
   - clear-output

 - stream-server branch
  - stream-server-usocket
   - methods
    - element-type => octet
  - socket-connect (socket-connect host port &key (protocol :stream)
           (element-type (quote character)) timeout deadline
           (nodelay t nodelay-specified) local-host local-port)
    - host can be name, string, vector, list or octets
   - socket-listen (host port &key
           (reuse-address NIL reuse-address-supplied-p)
           (backlog 5)
           (element-type (quote character)))
   - socket-accept (socket &key element-type)
   - socket-server (host port function &optional arguments &key in-new-thread (protocol :stream) (timeout 1)
           (max-buffer-size +max-datagram-packet-size+) element-type
           (reuse-address t) multi-threading name)
   - socket-shutdown (usocket direction)
    - ignore direction, can't close only half of a socket
    - maybe with stream-usocket-server stop listening but allow writes
   - stream-server-usocket-p (socket)

 -
   - stream-usocket class ?? what is this really?
    - methods
     - stream (can be used with setf)
    - stream-usocket-p (socket)
     - if io-stream and rw is net.Conn then t
   - datagram-usocket (maybe)


  - Vars
   + *auto-port* (default 0)
   + *wildcard-host* (default #(0 0 0 0))
   - *remote-host*
   - *remote-port*
  - functions
   - stream-usocket-p (socket)

   - vector-quad-to-dotted-quad (vector)
   - vector-to-ipv6-host (vector)
   - with-client-socket ((socket-var stream-var &rest socket-connect-args) &body body)
   - with-connected-socket ((var socket) &body body)
   - with-server-socket ((var server-socket) &body body)
   - with-socket-listener ((socket-var &rest socket-listen-args) &body body)

  - conditions/errors
   - socket-warning - a condition
   - socket-type-not-supported-error - a condition
   - timeout-error - a condition
   - unimplemented - a condition
   - unknown-condition - a condition
   - unknown-error - a condition
   - unsupported - a condition
   - socket-error - a condition
   - socket-condition - a condition


 - slot-value
  - add slot-value to flavors pkg/clos (for get and setf)
   - always allow


 - net package
  - implement sbcl networking or something closer to golang?
  - http://www.sbcl.org/manual/#Networking
  - https://www.quicklisp.org/beta/UNOFFICIAL/docs/usocket/doc/index.html
  - https://common-lisp-libraries.readthedocs.io/usocket/
  - socket
   - abstract flavor
   - Any
    - maybe struct with various options
     - Dialer - probably not, too much crap
     - Conn
     - other stuff as needed
    - connect then picks the correct Dial call
   - methods
    - bind
    - accept
    - connect
    - peername
    - name
    - receiver
    - send
    - listen
    - open-p
    - close
    - shutdown
    - make-stream
    - non-blocking-mode (question)
    - socket-error
    - all options
   - make flavor and target for generic functions
    - socket-bind and (send socket :bind &rest address)

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
