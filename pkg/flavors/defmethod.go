// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"github.com/ohler55/slip"
)

// DefmethodCall is the flavors implementation of defmethod.
func DefmethodCall(s *slip.Scope, ml, args slip.List) {
	var (
		flavor *Flavor
		daemon string
		method string
	)
	switch len(ml) {
	case 0, 1:
		slip.NewPanic("Too few elements in the method for defmethod. Expected 2 or 3 but got %d.", len(ml))
	case 2:
		if sym, ok2 := ml[0].(slip.Symbol); ok2 {
			flavor = allFlavors[string(sym)]
		}
	case 3:
		if sym, ok2 := ml[0].(slip.Symbol); ok2 {
			flavor = allFlavors[string(sym)]
		}
		if sym, ok2 := ml[1].(slip.Symbol); ok2 {
			daemon = string(sym)
		}
	default:
		slip.NewPanic("Too many elements in the method for defmethod. Expected 2 or 3 but got %d.", len(ml))
	}
	if sym, ok2 := ml[len(ml)-1].(slip.Symbol); ok2 && 1 < len(sym) && sym[0] == ':' {
		method = string(sym)
	} else {
		slip.PanicType("method for defmethod", ml[len(ml)-1], "keyword")
	}
	if flavor == nil {
		slip.PanicType("flavor for defmethod", ml[0], "name of flavor")
	}
	flavor.DefMethod(method, daemon, slip.DefLambda(method, s, args, flavor.varNames...))
}
