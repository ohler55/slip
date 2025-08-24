// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
)

// NoApplicableMethodErrorSymbol is the symbol with a value of "no-applicable-method-error".
const NoApplicableMethodErrorSymbol = Symbol("no-applicable-method-error")

// NoApplicableMethodErrorNew raises a no-applicable-method-error describing a
// no-applicable-method-error error.
func NoApplicableMethodErrorNew(s *Scope, depth int, gf Object, fargs List, format string, args ...any) Object {
	c := FindClass("no-applicable-method-error")
	obj := c.MakeInstance()

	argList := List{
		Symbol(":generic-function"), gf,
		Symbol(":function-arguments"), fargs,
	}
	if 0 < len(format) {
		argList = append(argList, Symbol(":message"), String(fmt.Sprintf(format, args...)))
	} else {
		var msg String
		if f, ok := gf.(Funky); ok {
			msg = String(fmt.Sprintf(`There is no applicable method for the generic function
#<generic-function %s>
  when called with arguments
    %s.`, f.GetName(), fargs))
		} else {
			msg = String(fmt.Sprintf(`There is no applicable method for the generic function
%s
  when called with arguments
    %s.`, gf, fargs))
		}
		argList = append(argList, Symbol(":message"), msg)
	}
	obj.Init(s, argList, depth)

	return obj
}

// NoApplicableMethodPanic raises a no-applicable-method-error describing a
// no-applicable-method-error error.
func NoApplicableMethodPanic(s *Scope, depth int, gf Object, fargs List, format string, args ...any) {
	panic(NoApplicableMethodErrorNew(s, depth, gf, fargs, format, args...))
}
