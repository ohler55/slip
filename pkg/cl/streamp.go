// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Streamp{Function: slip.Function{Name: "streamp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "streamp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "nil",
			Text:   `__streamp__ returns _true_ if _object_ is a stream.`,
			Examples: []string{
				`(setq out (make-string-inputstream)`,
				"(streamp out) => t",
				"(streamp t) => nil",
			},
		}, &slip.CLPkg)
}

// Streamp represents the streamp function.
type Streamp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Streamp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if _, ok := args[0].(slip.Stream); ok {
		return slip.True
	}
	return nil
}
