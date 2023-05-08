// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Stringp{Function: slip.Function{Name: "stringp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "stringp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "t|nil",
			Text:   `__stringp__ returns _true_ if _object_ is a string.`,
			Examples: []string{
				`(stringp "abc") => t`,
				"(stringp 5.1) => nil",
			},
		}, &slip.CLPkg)
}

// Stringp represents the stringp function.
type Stringp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Stringp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if _, ok := args[0].(slip.String); ok {
		return slip.True
	}
	return nil
}
