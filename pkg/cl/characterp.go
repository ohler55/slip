// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Characterp{Function: slip.Function{Name: "characterp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "characterp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "nil",
			Text:   `__characterp__ returns _true_ if _object_ is a character otherwise nil is returned.`,
			Examples: []string{
				`(characterp #\A) => t`,
				"(characterp 2) => nil",
			},
		}, &slip.CLPkg)
}

// Characterp represents the characterp function.
type Characterp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Characterp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if _, ok := args[0].(slip.Character); ok {
		return slip.True
	}
	return nil
}
