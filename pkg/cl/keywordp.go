// Copyright (c) 20223, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Keywordp{Function: slip.Function{Name: "keywordp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "keywordp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "nil",
			Text:   `__keywordp__ returns _true_ if _object_ is a keyword.`,
			Examples: []string{
				`(keywordp :abc) => t`,
				"(keywordp 'abc) => nil",
			},
		}, &slip.CLPkg)
}

// Keywordp represents the keywordp function.
type Keywordp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Keywordp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	if sym, ok := args[0].(slip.Symbol); ok && 1 < len(sym) && sym[0] == ':' {
		return slip.True
	}
	return nil
}
