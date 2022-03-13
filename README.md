# slip

SLIce Processing is LISP for golang



-------------------------------------------------------------------------------

- tests for base types


- lisp - SLIce Processing (slip)
 - name
  - gosp
  - gisp
  - glisp
  - golisp - taken
  - lispy
  - slice processing (sp) (slip)
  - list processing (lp)
 - list is reverse slice
 - support marcos
  - represented as scoped fun
  - support macro chars of , and ,@ in a backquoted (not single quote) list
   - , eval
   - ,@ is like: if foo is (a b) the ,@foo becomes a b
   - ', as is, no eval, (as string?)
   - https://lisp-journey.gitlab.io/blog/common-lisp-macros-by-example-tutorial/
   - defmacro defined should expand at compile time
   - golang macro should just not eval args

 - handle args of &rest and :key
 - what to call sexpr/**object**
 - object & class support
 - should a stack be used for eval instead of nested function calls?
