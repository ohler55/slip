// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// UnboundVariableSymbol is the symbol with a value of "unbound-variable".
const UnboundVariableSymbol = Symbol("unbound-variable")

// NewUnboundVariable returns a new UnboundVariablePanic (unbound-variable)
// describing a unbound-variable error.
func NewUnboundVariable(name Object, format string, args ...any) Object {
	c := FindClass("unbound-variable")
	obj := c.MakeInstance()

	obj.Init(NewScope(), List{
		Symbol(":name"), name,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, 0)
	return obj
}

// PanicUnboundVariable raises a UnboundVariablePanic (unbound-variable)
// describing a unbound-variable error.
func PanicUnboundVariable(name Object, format string, args ...any) {
	panic(NewUnboundVariable(name, format, args...))
}
