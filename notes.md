# SLIP Notes

- **multipass** or utm for linux

- next

 * [x] EVERY
 * [x] NBUTLAST
 * [x] NOTANY
 * [x] NOTEVERY
 * [x] SOME
 * [x] MAKE-LIST
 * [x] LIST-LENGTH
 * [x] COPY-ALIST
 * [x] COPY-LIST
 * [x] COPY-SEQ
 * [x] COPY-TREE
 * [x] POP
 * [x] PUSH
 * [x] PUSHNEW
 * [x] PSETF
 * [x] RANDOM-STATE-P
 * [x] RATIONALP
 * [x] PRINT-OBJECT
 * [x] PRINT-UNREADABLE-OBJECT - #<type addr>
 * [x] PAIRLIS
 * [x] SUBLIS
 * [x] NSUBLIS
 * [x] SUBSETP
 * [x] SUBST
 * [x] SUBST-IF
 * [x] NSUBST
 * [x] NSUBST-IF
 * [ ] SUBSTITUTE
 * [ ] SUBSTITUTE-IF
 * [ ] GET-PROPERTIES
 * [ ] GET
 * [ ] GETF
 * [ ] REMF
 * [ ] REMPROP
 * [ ] SYMBOL-PLIST
 * [ ] TAILP
 * [ ] LDIFF
 + gi:addnew


 - package-export
  - import
  - shadow
  - package-shadowing-symbols
  - shadowing-import

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
