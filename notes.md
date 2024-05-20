# SLIP Notes

- **multipass** or utm for linux

- next

 - package-export
  - maybe use VarVal as an Object
   - verify var needs to exist at defun time
   - add name/symbol to VarVal

  - Scope - TBDs
   - can this be set up in compile?
    - var lookup might need a func?

  - add Pkg to scope?
  - in Function eval and call set and revert scope Pkg setting

  - when evaluating, start with current package
   - for each eval add pkg to stack
    - pop off when returning
   - look in all packages for symbols
    - stack is not ideal unless can walk back up
    - need to avoid duplicates, map is a lot of overhead
   - maybe check on compile or is that just the ListToFunc call anyway
   - just ignore the export flag on eval? Assume lookup are done earlier (can they be?)


  - don't inherit (use) non-exported vars
   - then vars and funcs not exported are not available with the pkg and ::
   - if not found still need to check package of call/scope

  - add export flag to varval (private, noexport, internal, or export)
   - get var access working
    - maybe add func package to scope
   - figure out how to track package of current call
   - update export and unexport functions to handle variables as well

  - export
   - use find-symbol
    - update to return the correct values
   - support vars also

  - implement defpackage export list
   - keep list of exported and set func to be exported on defun or no if not on list
    - see what issues arise when user package since default will be not-exported
  - unexport
  - do-external-symbols
  - import



  - support export list and export function
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
   - inherit flos-flavor to generate functions for all method with a designated prefix
  - is a standard-class needed instead of vanilla or maybe just expand vanilla?

  - change-class (for flavors instances only for now)
   - parts of instance interface? same as class-of

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
