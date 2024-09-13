# SLIP Notes

- **multipass** or utm for linux

- next

 - usocket
   - change socket-state to use select with immediate timeout
   - functions
    + socket-close (usocket)
    + usocket-p (socket)
    + socket-state (socket)
    + get-local-name socket => address, port
    + get-local-address socket => address
    + get-local-port socket => port
    + get-peer-name socket => address, port
    + get-peer-address socket => address
    + get-peer-port socket => port
    + socket-send (usocket buffer length &key host port offset timeout)
    + socket-receive (usocket buffer &optional length &key timeout)
    + socket-stream (socket)
    + socket-option
    + wait-for-input
    - socket-select (read write error &key timeout)
    - socket-pair (domain type protocol &key nonblock cloexec)
     - maybe set up constants for the various values or a map for run time lookup
     - domain - :local or :unix - AF_UNIX
              - :inet - AF_INET
              - :inet6 - AF_INET6
	          AF_ALG                           = 0x26
	          AF_APPLETALK                     = 0x5
	          AF_ASH                           = 0x12
	          AF_ATMPVC                        = 0x8
	          AF_ATMSVC                        = 0x14
	          AF_AX25                          = 0x3
	          AF_BLUETOOTH                     = 0x1f
	          AF_BRIDGE                        = 0x7
	          AF_CAIF                          = 0x25
	          AF_CAN                           = 0x1d
	          AF_DECnet                        = 0xc
	          AF_ECONET                        = 0x13
	          AF_FILE                          = 0x1
	          AF_IEEE802154                    = 0x24
	          AF_INET                          = 0x2
	          AF_INET6                         = 0xa
	          AF_IPX                           = 0x4
	          AF_IRDA                          = 0x17
	          AF_ISDN                          = 0x22
	          AF_IUCV                          = 0x20
	          AF_KEY                           = 0xf
	          AF_LLC                           = 0x1a
	          AF_LOCAL                         = 0x1
	          AF_MAX                           = 0x27
	          AF_NETBEUI                       = 0xd
	          AF_NETLINK                       = 0x10
	          AF_NETROM                        = 0x6
	          AF_PACKET                        = 0x11
	          AF_PHONET                        = 0x23
	          AF_PPPOX                         = 0x18
	          AF_RDS                           = 0x15
	          AF_ROSE                          = 0xb
	          AF_ROUTE                         = 0x10
	          AF_RXRPC                         = 0x21
	          AF_SECURITY                      = 0xe
	          AF_SNA                           = 0x16
	          AF_TIPC                          = 0x1e
	          AF_UNIX                          = 0x1
	          AF_UNSPEC                        = 0x0
	          AF_WANPIPE                       = 0x19
	          AF_X25                           = 0x9
     - type
              SOCK_CLOEXEC                     = 0x80000 (flag to or with others)
	          SOCK_DCCP                        = 0x6
	          SOCK_DGRAM                       = 0x2
	          SOCK_NONBLOCK                    = 0x800 (flag to or with others)
	          SOCK_PACKET                      = 0xa
	          SOCK_RAW                         = 0x3
	          SOCK_RDM                         = 0x4
	          SOCK_SEQPACKET                   = 0x5
	          SOCK_STREAM                      = 0x1
     - protocol
              IPPROTO_AH                       = 0x33
	          IPPROTO_COMP                     = 0x6c
	          IPPROTO_DCCP                     = 0x21
	          IPPROTO_DSTOPTS                  = 0x3c
	          IPPROTO_EGP                      = 0x8
	          IPPROTO_ENCAP                    = 0x62
	          IPPROTO_ESP                      = 0x32
	          IPPROTO_FRAGMENT                 = 0x2c
	          IPPROTO_GRE                      = 0x2f
	          IPPROTO_HOPOPTS                  = 0x0
	          IPPROTO_ICMP                     = 0x1
	          IPPROTO_ICMPV6                   = 0x3a
	          IPPROTO_IDP                      = 0x16
	          IPPROTO_IGMP                     = 0x2
	          IPPROTO_IP                       = 0x0
	          IPPROTO_IPIP                     = 0x4
	          IPPROTO_IPV6                     = 0x29
	          IPPROTO_MTP                      = 0x5c
	          IPPROTO_NONE                     = 0x3b
	          IPPROTO_PIM                      = 0x67
	          IPPROTO_PUP                      = 0xc
	          IPPROTO_RAW                      = 0xff
	          IPPROTO_ROUTING                  = 0x2b
	          IPPROTO_RSVP                     = 0x2e
	          IPPROTO_SCTP                     = 0x84
	          IPPROTO_TCP                      = 0x6
	          IPPROTO_TP                       = 0x1d
	          IPPROTO_UDP                      = 0x11
	          IPPROTO_UDPLITE                  = 0x88

 - stream-server branch
  - stream-server-usocket
   - methods
    - element-type (octet)
  - socket-connect (socket-connect host port &key (protocol :stream)
           (element-type (quote character)) timeout deadline
           (nodelay t nodelay-specified) local-host local-port &aux
           (sockopt-tcp-nodelay-p (fboundp (quote sockopt-tcp-nodelay))))

 -
   - stream-usocket class ?? what is this really?
    - methods
     - stream (can be used with setf)
    - stream-usocket-p (socket)
     - if io-stream and rw is net.Conn then t
   - datagram-usocket (maybe)


   - ipaddr (host)
    - fixnum (32 bit)
    - 4 elememnt list or vector #(127 0 0 1)
    - string in dotted format
    - hostname
    - add ipv6 support
     - string
     - fixnum (64 bit)
    - socket is an api


  - Vars
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
