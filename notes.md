# SLIP Notes

- **multipass** or utm for linux

- next
---------------------

update readme
 - split up
  - readme just for summary (with link to features), install, releases, links
  - features.md

- grahql
 - graphql-query (url query &optional headers &key protocol method)
  - query - field(args){subs}
   - field
    "quux"
    ("quux" args subs)
   - args
    (("arg-name" arg-value)...)
   - arg-value
    123
    "string"
    1.25
    (enum "string")
    - input-value
     (type ("f0" arg-value)...)
   - subs
    - field list
  - query can also be a string
 - graphql-mutation (url query &optional headers &key protocol)
 - alternatives (better)
  - provide a template and fill then provide values
   - just use format ?? with slip extensions ~=
   - graphql-query (url template &optional headers &key protocol template-args :allow-other-keys)
    - use (format nil template template-args)
     - note all keys are already assigned and ~= can be used
    - always a POST
    - returns a bag

graphql-query (url template &optional headers &key content-type template-args :allow-other-keys)

Sends a GraphQL POST request to _url_. Responses are returned as a
__bag__ instance. The template provided is used as the _control_
argument to a call to the __format__ function. The &key arguments can
be used with the ~= directive which is a __slip__ extension to the
__format__ directives.

The _:content-type_ can be either :graphql, the default, or :json. If
:graphql then the Content-Type header is set to
application/graphql. If :json then the Content-Type is set to
application/json. The content is set accordingly.

example:
 (graphql-query
   "http://example.com/graphql"
   "user(id:~S group:\"~=group~=\") {name}"
   :template-args "id-123"
   :group "one")
 {data:{user:{name:"Fred"}}}


flavor define order

---------------------

- for later
 - [ ] WITH-ACCESSORS
 - [ ] ENSURE-GENERIC-FUNCTION
 - [ ] REINITIALIZE-INSTANCE
 - [ ] UPDATE-INSTANCE-FOR-REDEFINED-CLASS
 - [ ] CHANGE-CLASS - call generic update-instance-for-different-class

  - clos https://lispcookbook.github.io/cl-cookbook/clos.html and https://www.algo.be/cl/documents/clos-guide.html

-------------------
 - flavor allow out of order defflavor like standard-class
 - rename bag-flavor to just bag?

-----------------
  - [ ] inspect [interactive]

 - tough-ones
  - [ ] DESTRUCTURING-BIND
  - [ ] FORMATTER
  - [ ] INTEGER-DECODE-FLOAT

  - allow (coerce '(1 0 1 0) '(vector (integer 0 1) 4)) - in the future
   - might need a type-spec type to use
    - TypeSpec interface
     - check method that panics on fail
     - isOk method for true or false
      - separate for each type like integer, float, etc
       - IntegerSpec - low, high

 - watch.connect.safeEval
  - encode condition and decode
  - frame test broken


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


 - future repl options
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
