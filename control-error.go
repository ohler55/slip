// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ControlErrorSymbol is the symbol with a value of "control-error".
const ControlErrorSymbol = Symbol("control-error")

// NewControlError creates a ControlPanic (control-error) describing a control error.
func NewControlError(format string, args ...any) Object {
	c := FindClass("control-error")
	obj := c.MakeInstance()

	obj.Init(NewScope(), List{
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, 0)
	return obj
}

// PanicControl raises a ControlPanic (control-error) describing a control
// error.
func PanicControl(format string, args ...any) {
	panic(NewControlError(format, args...))
}
