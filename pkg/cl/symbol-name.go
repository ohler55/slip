// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SymbolName{Function: slip.Function{Name: "symbol-name", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "symbol-name",
			Args: []*slip.DocArg{
				{
					Name: "symbol",
					Type: "symbol",
					Text: "The symbol to return the name of.",
				},
			},
			Return: "string|nil",
			Text:   `__symbol-name__ returns the name of _symbol_.`,
			Examples: []string{
				`(symbol-name 'aBc) => "abc"`,
				`(symbol-name :xYz) => "xyz"`,
			},
		}, &slip.CLPkg)
}

// SymbolName represents the symbol-name function.
type SymbolName struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SymbolName) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "symbol", args[0], "symbol")
	}
	str := string(sym)
	if 0 < len(str) && str[0] == ':' {
		str = str[1:]
	}
	return slip.String(strings.ToLower(str))
}
