// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Error{Function: slip.Function{Name: "error", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "error",
			Args: []*slip.DocArg{
				{
					Name: "control",
					Type: "string",
					Text: `The control string for the errorting.`,
				},
				{Name: "&rest"},
				{
					Name: "arguments",
					Type: "object",
					Text: "The arguments to be used as arguments to the _control_ string.",
				},
			},
			Text: `__error__ raises an error with the text formed from the _control_ and _arguments_.`,
			Examples: []string{
				`(error "sample failed for ~A" 123) ;; panics`,
			},
		}, &slip.CLPkg)
}

// Error represents the error function.
type Error struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Error) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)

	panic(slip.NewError("%s", FormatArgs(s, args)))
}
