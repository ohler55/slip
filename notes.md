# SLIP Notes

- **multipass** or utm for linux

- next
 - add to gi
  - directory-exists-p (path)
  - file-info (path) => assoc
   - name, size, mode, mod-time, is-dir
  - glob (pattern) => list
  - map-glob (function pattern)
  - walk-directory (function directory)
   - function (path info err) => bool
    - if returns t then skip dir
  - file-kind (path &key follow-symlinks)
   - :regular-file, :symbolic-link, :directory, :pipe, :socket, :character-device, :block-device or nil if does not exist
  - file-permissions
   - :user-read, :user-write, :user-exec, :group-read, :group-write, :group-exec, :other-read, :other-write, :other-exec, :set-user-id, :set-group-id, :sticky.


 - structs - seems like a downgrade from class or flavors, just another weaker alternative instances with slots

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


 - clos methods/generics (flavors and clos mix as flos)
  - flos
   - defgeneric is only used to check defmethod
   - inherit flos-flavor to generate functions for all method with a designated prefix
  - is a standard-class needed instead of vanilla or maybe just expand vanilla?

  - change-class (for flavors instances only for now)
   - parts of instance interface? same as class-of

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
