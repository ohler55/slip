# SLIP Notes

- **multipass** or utm for linux

- next
 - watch tests

 - return and return-from should work in functions (defun)

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
  - use-package
  - in-package
  - package-name
  - package-nicknames
  - rename-package
  - shadow
  - package-shadowing-symbols
  - shadowing-import
  - export
  - unexport
  - package-used-by-list
  - unuse-package
  - use-package
  - package-use-list
  - require (with lisp code)
  - do-symbols
  - do-all-symbols
  - do-external-symbols
  - find-symbol (string|symbol &optional package) => symbol, status
   - status
    - :internal - in package
    - :external - what does this mean? maybe when package provide it is external?
     - if exported to another package
    - :inherited - through use-package
   - return nil, nil if not present
   - find func or var
   - sbcl is case sensitive, maybe don't be
  - find-all-symbols  (string|symbol)
   - all packages search and return list of symbols
    - symbols not in current package should be printed with package
     - create the symbol with the package

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

 - clos methods/generics (flavors and clos mix as flos)
  - flos
   - defgeneric is only used to check defmethod
   + flosfun to create a wrapper around send :xxx
   - maybe a a key in function doc string to link method to function
   - inherit flos-flavor to generate functions for all method with a designated prefix
  - possibly add a flag indicating the flavor is a class vs flavor or flos
  - is a standard-class needed instead of vanilla or maybe just expand vanilla?

  - change-class (for flavors instances only for now)
   - parts of instance interface? same as class-of

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


- base64 (encode and decode in gi package)

 - other method combinations?
  - :method-combination option for defflavor
  - daemon (default)
  - progn
  - or
  - and
  - list

- ui with fyne
