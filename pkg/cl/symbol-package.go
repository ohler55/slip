// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SymbolPackage{Function: slip.Function{Name: "symbol-package", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "symbol-package",
			Args: []*slip.DocArg{
				{
					Name: "symbol",
					Type: "symbol",
					Text: "The symbol to return the package of.",
				},
			},
			Return: "nil",
			Text:   `__symbol-package__ returns the package of _symbol_.`,
			Examples: []string{
				`(symbol-package 'car)) => #<package car>`,
			},
		}, &slip.CLPkg)
}

// SymbolPackage represents the symbol-package function.
type SymbolPackage struct {
	slip.Function
}

// Call the package with the arguments provided.
func (f *SymbolPackage) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "symbol", args[0], "symbol")
	}
	if fn := slip.CurrentPackage.GetFunc(string(sym)); fn != nil {
		return fn.Pkg
	}
	if vv := slip.CurrentPackage.GetVarVal(string(sym)); vv != nil {
		return vv.Pkg
	}
	if index := strings.IndexByte(string(sym), ':'); 0 < index {
		str := string(sym)
		if pkg := slip.FindPackage(str[:index]); pkg != nil {
			return pkg
		}
	}
	return slip.CurrentPackage
}
