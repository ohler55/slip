# SLIP Notes

- **multipass** or utm for linux

- next

 - package-export
  - import
  - shadow
  - package-shadowing-symbols
  - shadowing-import

 - flavor
  - add non-standard doc for var or method
   - method on flavor :document name description
   - also a function
 - gi package
  - defsystem returns a system instance, can also use make-instance
  - system - flavor
    - note differences to ASDF
     - not all source in one place, avoids version conflicts
     - relies on an import cache where depends-on systems and files are placed
     - simplified components as just file names, no module support, use another system
     - depends-on identifies sources of code
      - directory or file list ;; (:file a b c d)
      - git branch ;; (:git url
      - git tag
      - git commit
      - another system
    - :in-order-to has the same purpose but is slightly different
    - has pre-defined operations
     - :fetch to fetch depends-on and place them in the cache
   - vars - inittable and gettable but not settable
    - :author
    - :maintainer
    - :license "MIT"
    - :version
    - :homepage "https://github.com/CommonDoc/common-doc"
    - :bug-tracker "https://github.com/CommonDoc/common-doc/issues"
    - :source-control (:git "git@github.com:CommonDoc/common-doc.git")
    - :description "A framework for representing and manipulating documents as CLOS objects."
    - :depends-on ;;
    - :components  ;; just list of files with or without .lisp
    - :in-order-to '((:test (run-my-tests 1 2))
                     (:run (run-stuff)))
    - :scratch ;; not asdf
    - :cache ;; not asdf

  - source
   - (source-name source &key cache-dir)
    - source - (:git url &key branch commit tag sub-dir)
    -          (:file filepath*)
    -          (:system path)
    -          (:call (some-lisp-function and args))

 - block-comment branch
  - block comments #|  |#

- system branch
 - system (like ASDF)
  - defsystem
   - https://lisp-lang.org/learn/writing-libraries
   - https://asdf.common-lisp.dev/
  - types/flavors
   - system
    - :load
    - :install or :cache or :prepare
    - in gi package?
   - source (maybe not needed if source spec is clear)
    - file
    - git (tag or branch)

  - support a config file with a source or source-registry path
  - how about allowing .asdf files?

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
