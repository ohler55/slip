// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MethodCombinationError{
				InvalidMethodError: InvalidMethodError{
					Function: slip.Function{Name: "method-combination-error", Args: args},
				},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "method-combination-error",
			Args: []*slip.DocArg{
				{
					Name: "method",
					Type: "symbol",
					Text: "The method the error is associated with.",
				},
				{
					Name: "format-control",
					Type: "string",
					Text: "The format control string for the error message.",
				},
				{Name: "&rest"},
				{
					Name: "args",
					Type: "object",
					Text: `Arguments to the _format-control_.`,
				},
			},
			Return: "method-error",
			Text: `__method-combination-error__ makes a _method-error_ initialized with the _method_ and a
string generated from the _format-control_ and _args_.`,
			Examples: []string{
				`(method-combination-error :mess "test") => #<METHOD-ERROR 12345>`,
			},
		}, &slip.CLPkg)
}

// MethodCombinationError represents the method-combination-error function.
type MethodCombinationError struct {
	InvalidMethodError
}
