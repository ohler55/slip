# Generics

TBD

- difference from other object systems
 - methods not attached to types
 - no name isolation, all global so more likely to have collisions unless prefix or longer names (the normal approach in lisp)

- leverage method struct from flavors
 - could have been more specialize but a fair amount of overlap and names are the same
- cache methods
 - defmethod invalidates cache
- use regular package based function lookup (funcinfo)
- pretty-print and snapshot uses progn
