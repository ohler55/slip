// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SymbolFunction{Function: slip.Function{Name: "symbol-function", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "symbol-function",
			Args: []*slip.DocArg{
				{
					Name: "symbol",
					Type: "symbol",
					Text: "The symbol to return the function of.",
				},
			},
			Return: "nil",
			Text:   `__symbol-function__ returns the function of _symbol_.`,
			Examples: []string{
				`(symbol-function 'car)) => #<function car>`,
			},
		}, &slip.CLPkg)
}

// SymbolFunction represents the symbol-function function.
type SymbolFunction struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SymbolFunction) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "symbol", args[0], "symbol")
	}
	fn := slip.CurrentPackage.GetFunc(string(sym))
	if fn == nil {
		slip.PanicUndefinedFunction(sym, "The function %s:%s is undefined.", slip.CurrentPackage.Name, sym)
	}
	return fn
}
