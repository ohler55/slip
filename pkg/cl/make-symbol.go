// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeSymbol{Function: slip.Function{Name: "make-symbol", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-symbol",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "string",
					Text: `The string to make into a symbol.`,
				},
			},
			Return: "symbol",
			Text:   `__make-symbol__ returns a new symbol from _name_.`,
			Examples: []string{
				`(make-symbol "abc") => abc`,
			},
		}, &slip.CLPkg)
}

// MakeSymbol represents the make-symbol function.
type MakeSymbol struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeSymbol) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	str, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("name", args[0], "string")
	}
	return slip.Symbol(str)
}
