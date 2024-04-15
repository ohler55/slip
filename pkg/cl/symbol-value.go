// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SymbolValue{Function: slip.Function{Name: "symbol-value", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "symbol-value",
			Args: []*slip.DocArg{
				{
					Name: "symbol",
					Type: "symbol",
					Text: "The symbol to return the value of.",
				},
			},
			Return: "nil",
			Text:   `__symbol-value__ returns the value of _symbol_.`,
			Examples: []string{
				`(let ((x 2)) (symbol-value 'x)) => 2`,
			},
		}, &slip.CLPkg)
}

// SymbolValue represents the symbol-value function.
type SymbolValue struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SymbolValue) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.PanicType("symbol", args[0], "symbol")
	}
	if 0 < len(sym) && sym[0] == ':' {
		return sym
	}
	result, has := slip.CurrentPackage.Get(string(sym))
	if !has {
		slip.PanicUnboundVariable(sym, "The variable %s is unbound.", sym)
	}
	return result
}
