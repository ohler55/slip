// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// UnboundVariableSymbol is the symbol with a value of "unbound-variable".
const UnboundVariableSymbol = Symbol("unbound-variable")

// UnboundVariableNew returns a new UnboundVariable (unbound-variable)
// describing a unbound-variable error.
func UnboundVariableNew(s *Scope, depth int, name Object, format string, args ...any) Object {
	c := FindClass("unbound-variable")
	obj := c.MakeInstance()

	obj.Init(s, List{
		Symbol(":name"), name,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, depth)
	return obj
}

// UnboundVariablePanic raises a UnboundVariable (unbound-variable) describing
// a unbound-variable error.
func UnboundVariablePanic(s *Scope, depth int, name Object, format string, args ...any) {
	panic(UnboundVariableNew(s, depth, name, format, args...))
}
