# SLIP Notes

- **multipass** or utm for linux

- next

 - allow (make-array 3) // dims as fixnum to make a vector

 * [x] MAKE-ARRAY
 * [ ] AREF (also placer with setf)
 * [ ] VECTOR-POP - only for vector with a fill pointer
 * [ ] VECTOR-PUSH - only for vector with a fill pointer
 * [ ] VECTOR-PUSH-EXTEND - only for vector with a fill pointer
 * [ ] ADJUST-ARRAY - can modify all fields of array
 * [ ] ADJUSTABLE-ARRAY-P
 * [ ] ARRAY
 * [ ] ARRAY-DIMENSION
 * [ ] ARRAY-DIMENSION-LIMIT
 * [ ] ARRAY-DIMENSIONS
 * [ ] ARRAY-DISPLACEMENT
 * [ ] ARRAY-ELEMENT-TYPE
 * [ ] ARRAY-HAS-FILL-POINTER-P
 * [ ] ARRAY-IN-BOUNDS-P
 * [ ] ARRAY-RANK - number of dims
 * [ ] ARRAY-RANK-LIMIT
 * [ ] ARRAY-ROW-MAJOR-INDEX
 * [ ] ARRAY-TOTAL-SIZE
 * [ ] ARRAY-TOTAL-SIZE-LIMIT
 * [ ] ARRAYP
 * [ ] FILL-POINTER - works with setf
 * [ ] SIMPLE-ARRAY - all slip arrays are currently simple
 * [ ] SIMPLE-VECTOR - fill pointer is nil
 * [ ] SIMPLE-VECTOR-P - t if no fill pointer (how about expressly adjustable?)
 * [ ] UPGRADED-ARRAY-ELEMENT-TYPE
 * [ ] SVREF
 * [ ] ROW-MAJOR-AREF

 - package
  - support export list
   - ListToFunc should be f.ListToFunc
    - lookup of func should consider the package of f
     - if lookup is in f.pkg or lookup is exported (new flag) then ok
    - add export flag to FuncInfo
    - Define() and package.Define() need extra arg for export or not
     - maybe default to export
     - flag in FuncDoc or separate? depends on whether it's useful in docs

  - do-external-symbols [same as do-symbols for now or until export is implemented]
  - shadow
  - package-shadowing-symbols
  - shadowing-import
  - export
  - unexport
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


 - array
  - add fill-pointer for one dimensional arrays
 - vector-pop - takes 1 dimensional arrays, fails on simple vector or array
 - vector-push - ...
 - vector-push-extend - ...

 - merge (starts with result-type)

 - plist (property lists) - tied to a symbol which makes it global, less useful as such
  - get (also a placer)
  - getf (same as get?)
  - get-properties
  - remprop
  - remf
  - symbol-plist
  - in gi package
   - put (non-standard)
   - plistp
   - plist-value
   - plist-values
   - plist-keys
   - doplist

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
