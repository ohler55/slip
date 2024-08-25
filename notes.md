# SLIP Notes

- **multipass** or utm for linux

- next

 - usocket in pkg/net
  - separate files for methods (start with state) (like flow/task-height)
  - classes
   - usocket
    - :close
    - :local-name
    - :local-address
    - :local-port
    - :peer-name
    - :peer-address
    - :peer-port
    - :send
    - :receive
    - :wait-for-input
    - :stream
    - :option
   - functions
    - usocket-p (socket)
    - stream-usocket-p (socket)
    - socket-state (socket)
    - get-local-name socket => address, port
    - get-local-address socket => address
    - get-local-port socket => port
    - get-peer-name socket => address, port
    - get-peer-address socket => address
    - get-peer-port socket => port
    - socket-send (usocket buffer length &key host port offset)
    - socket-receive (usocket buffer length &key element-type)
    - wait-for-input (socket-or-sockets &key timeout ready-only &aux (single-socket-p (usocket-p socket-or-sockets)))
    - socket-stream (socket)
    - socket-option (socket option &key)

   - stream-usocket class
    - methods
     - stream (can be used with setf)
   - stream-server-usocket
    - methods
     - element-type (byte?)
   - datagram-usocket (maybe)

  - plan
   - create stream-usocket flavor
    - socket-stream (object) => stream-usocket
    - make-instance
    -

   - ipaddr (host)
    - fixnum (32 bit)
    - 4 elememnt list or vector #(127 0 0 1)
    - string in dotted format
    - hostname
    - add ipv6 support
     - string
     - fixnum (64 bit)
    - socket is an api


  - vars
   - *auto-port* (default 0)
   - *remote-host*
   - *remote-port*
   - *wildcard-host* (default #(0 0 0 0))
  - functions
   - socket-connect (host port &key
           (protocol :stream)
           (element-type (quote character)) timeout deadline
           (nodelay t nodelay-specified) local-host local-port)
   - socket-listen (host port &key
           (reuse-address NIL reuse-address-supplied-p)
           (backlog 5)
           (element-type (quote character)))
   - socket-accept (socket &key element-type)
   - socket-close (usocket)
   - get-local-name socket => address, port
   - get-local-address socket => address
   - get-local-port socket => port
   - get-peer-name socket => address, port
   - get-peer-address socket => address
   - get-peer-port socket => port
   - socket-send (usocket buffer length &key host port offset)
   - socket-receive (usocket buffer length &key element-type)
   - wait-for-input (socket-or-sockets &key timeout ready-only &aux (single-socket-p (usocket-p socket-or-sockets)))
   - socket-server (host port function &optional arguments &key in-new-thread (protocol :stream) (timeout 1)
           (max-buffer-size +max-datagram-packet-size+) element-type
           (reuse-address t) multi-threading name)
   - stream-server-usocket-p (socket)
   - stream-usocket-p (socket)
   - usocket-p (socket)
   - vector-quad-to-dotted-quad (vector)
   - vector-to-ipv6-host (vector)
   - with-client-socket ((socket-var stream-var &rest socket-connect-args) &body body)
   - with-connected-socket ((var socket) &body body)
   - with-server-socket ((var server-socket) &body body)
   - with-socket-listener ((socket-var &rest socket-listen-args) &body body)
   - socket-stream (socket)
   - socket-state (socket)
   - socket-shutdown (usocket direction)
   - socket-option (socket option &key)
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
  - https://marketsplash.com/tutorials/lisp/lisp-network-programming/
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
