# SLIP Notes

- **multipass** or utm for linux

- next

 - package-export
  - import
  - shadow
  - package-shadowing-symbols
  - shadowing-import

 - reader-speed
  - gi:memstat () => assoc list of stats
  - gi:gc - force a gc and wait with runtime.GC()
  - Code.ReadStream(r io.Reader) (code Code)
  - Code.ReadOneStream(r io.Reader) (code Code)
  - block-comment branch
   - block comments #|  |#
  - cl:read
   - change to use code read from stream
    - add code func to read from stream Code.ReadInput or ReadStream or Load?
     - load has different meaning in list so better to use ReadStream

 - fast save and load
  - save (object filepath)
   - try with stripped down Printer copy else just printer with options set
  - make read or code.read support streams
   - leave basically the same but for stream have a thread that fetchs a channel with next
    - need a flag indicating not yet eof
    - need to handle mid-object read, maybe with a temp buffer
    - give the read buffer back to file reader for the next read (channel)
    - use (room) to check memory

  - as lisp with fastest print options (save (object &optional stream))
   -  Printer{
		ANSI:        false,
		Array:       false,
		Base:        10,
		Case:        downcaseKey,
		Circle:      false,
		Escape:      true,
		Gensym:      true,
		Lambda:      true,
		Length:      math.MaxInt,
		Level:       math.MaxInt,
		Lines:       math.MaxInt,
		Prec:        -1,
		MiserWidth:  0,
		Pretty:      false,
		Radix:       false,
		Readably:    false,
		RightMargin: 0,
	}
  - as binary
   - encode all types as type, length, data
    - is a byte size needed?
     - maybe not since no memory is allocated explicitly
    - instances?

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
