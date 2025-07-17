// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// WarningSymbol is the symbol with a value of "serious-condition".
const WarningSymbol = Symbol("warning")

// NewWarning returns a Warning object.
func NewWarning(format string, args ...any) Object {
	c := FindClass("warning")
	obj := c.MakeInstance()

	obj.Init(NewScope(), List{
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, 0)
	return obj
}
