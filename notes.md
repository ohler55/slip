# SLIP Notes

- **multipass** or utm for linux

- next

 - messaging
  - body can be json, sen, or lisp. If first character is ( then lisp else sen
  - msg-hub - abstract for message hub
  - nats-hub-flavor
  - jetstream-hub-flavor
  - mem-hub-flavor
  - subject configuration out of band possibly
   - will a simple string be enough or are jetstream variations needed?
  - publish or send is always the same
   - handling is configured out of band or through the hub
  - listen with callback
  - (:get subject) for queues
  - explicit ack if configured


 - xml
  - directly to lisp
   - (element1 element2)
   - element: (name attrs children...)
   - attrs: assoc list
  - callback SAX option also using a SAX-flavor with methods for each callback
  - xml-read
   - input input-stream
  -
  - xml-write
   - destination (stream, nil, t)
   - data

 - flow package
  - classes/flavors
   - task-flavor
    - methods
     - start
      - starts processing loop
     - stop
     - submit box/data/bag
      - drops data on to processing channel
      - initially copy but later wrap with box that dups on set
       - or maybe enhance bag to have option for copy on set (immuttable flag)
     - handle-error
     - flow return flow task is in
     - links - returns link names with task as assoc list
    - subclass for specific behavior
  - flow-flavor
   - init should take a config but allow for changes later
   - can subclass for specific flows
   - methods
     - start (starts all tasks)
     - stop &optional wait (all tasks)
    - submit data &optional wait
    - handle-error
    - logger field points to gi/logger
    - tasks
    - add-task
    - set-entry
    - link name task (or task-name)
     - optional flow for external links
   - vars
    - entry
    - tasks
  - syntax for describing, json or lisp

 - parquet writer - future
  - parquet package (https://github.com/apache/parquet-format)
   - https://pkg.go.dev/github.com/apache/arrow/go/v13@v13.0.0/parquet#Version
   - https://github.com/apache/parquet-testing/tree/master/data
   - https://platform.opentargets.org/downloads
  - options
   - with metadata
   - with props
  - methods
   - close
   - row-count
   - column-count
   - row-group-count
   - properties
  - if all, use ReadTable() or is each column just as good?

 - net package
  - implement sbcl networking or something closer to golang?
  - http://www.sbcl.org/manual/#Networking
  - https://marketsplash.com/tutorials/lisp/lisp-network-programming/
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


 - graphql (part of net or separate?)
  - client
  - server

 - package
  - support export list
   - ListToFunc should be f.ListToFunc
    - lookup of func should consider the package of f
     - if lookup is in f.pkg or lookup is exported (new flag) then ok
    - add export flag to FuncInfo
    - Define() and package.Define() need extra arg for export or not
     - maybe default to export
     - flag in FuncDoc or separate? depends on whether it's useful in docs


  - defpackage
  - in-package
  - package-name
  - package-nicknames
  - rename-package
  - shadow
  - package-shadowing-symbols
  - shadowing-import
  - intern
  - unintern
  - find-symbol
  - export
  - unexport
  - package-used-by-list
  - unuse-package
  - use-package
  - package-use-list
  - require
  - find-all-symbols
  - do-symbols
  - do-all-symbols
  - do-external-symbols


 - clos methods/generics
  - change-class (for flavors instances only for now)
   - parts of instance interface? same as class-of
  - generic functions and methods
   - GenericCaller
    - defines an expected argument set that gets checked first
    - keep a collection that matches argument types with methods
     - list and pick best so far until end
     - or nested maps
      - type check has to look at inheritance so maybe not that helpful to have a map
    - method is like flavors, uses a class preference list
     - list is from type/class and is built on defmethod
     - what does it mean to have before and after on multiple type functions?
  - class slots

 - trace
  - should trace by function be supported instead of overall trace?
   - maybe (trace t) to turn on all as currently implemented and nil to turn off
    - (untrace) turns off all
   - move trace to pkg/cl
   - function names (specs) can be symbol, string, (METHOD name qualifiers) - qualifiers could be flavor and daemon?
    - for methods
     - special case for :send and look at sendMap
      - method then flavor sub-map
      - or submap for daemon before that?
      - or maybe method includes daemon (:before:foo as key)


 - array
  - add fill-pointer for one dimensional arrays
 - vector-pop - takes 1 dimensional arrays, fails on simple vector or array
 - vector-push - ...
 - vector-push-extend - ...

 - merge (starts with result-type)
 - room

 - property lists
  - can modify but not add to or remove as list changes
  - getf for existing only
  - setf for existing only
  - remf replace key and value with nil


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

 - coerce
  - also support integers unlike CL
  - only support symbols (later lists like '(integer 3 5) or '(and list (not null)) )


 - macro
  - expand on read

- base64 (encode and decode in gi package)

 - other method combinations?
  - :method-combination option for defflavor
  - daemon (default)
  - progn
  - or
  - and
  - list


- would building our own stack be a better approach?
 - could reuse scope but would need to clear vars
 - could replace args key lookups for lispcaller with an index to arg to avoid using maps


- support marcos
 - represented as scoped fun
 - support macro chars of , and ,@ in a backquoted (not single quote) list
  - , eval
  - ,@ is like: if foo is (a b) the ,@foo becomes a b
  - ', as is, no eval, (as string?)
  - https://lisp-journey.gitlab.io/blog/common-lisp-macros-by-example-tutorial/
  - defmacro defined should expand at compile time
  - golang macro should just not eval args
