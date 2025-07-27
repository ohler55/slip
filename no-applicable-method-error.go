// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
)

// NoApplicableMethodErrorSymbol is the symbol with a value of "no-applicable-method-error".
const NoApplicableMethodErrorSymbol = Symbol("no-applicable-method-error")

// NewNoApplicableMethodError raises a NoApplicableMethodPanic (no-applicable-method-error)
// describing a no-applicable-method-error error.
func NewNoApplicableMethodError(gf Object, fargs List, format string, args ...any) Object {
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
	obj.Init(NewScope(), argList, 0)

	return obj
}

// PanicNoApplicableMethodError raises a MethodPanic (no-applicable-method-error)
// describing a no-applicable-method-error error.
func PanicNoApplicableMethod(gf Object, fargs List, format string, args ...any) {
	panic(NewNoApplicableMethodError(gf, fargs, format, args...))
}
