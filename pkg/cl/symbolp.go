// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Symbolp{Function: slip.Function{Name: "symbolp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "symbolp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "nil",
			Text:   `__symbolp__ returns _true_ if _object_ is a symbol.`,
			Examples: []string{
				`(symbolp 'abc) => t`,
				"(symbolp 5.1) => nil",
			},
		}, &slip.CLPkg)
}

// Symbolp represents the symbolp function.
type Symbolp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Symbolp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if _, ok := args[0].(slip.Symbol); ok {
		return slip.True
	}
	return nil
}
