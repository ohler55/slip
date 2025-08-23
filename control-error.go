// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ControlErrorSymbol is the symbol with a value of "control-error".
const ControlErrorSymbol = Symbol("control-error")

// ControlErrorNew creates a ControlError (control-error) describing a control
// error.
func ControlErrorNew(s *Scope, depth int, format string, args ...any) Object {
	c := FindClass("control-error")
	obj := c.MakeInstance()

	obj.Init(s, List{
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, depth)
	return obj
}

// ControlPanic raises a ControlError (control-error) describing a control
// error.
func ControlPanic(s *Scope, depth int, format string, args ...any) {
	panic(ControlErrorNew(s, depth, format, args...))
}
