// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := InvalidMethodError{Function: slip.Function{Name: "invalid-method-error", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "invalid-method-error",
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
			Text: `__invalid-method-error__ makes a _method-error_ initialized with the _method_ and a
string generated from the _format-control_ and _args_.`,
			Examples: []string{
				`(invalid-method-error :mess "test") => #<METHOD-ERROR 12345>`,
			},
		}, &slip.CLPkg)
}

// InvalidMethodError represents the invalid-method-error function.
type InvalidMethodError struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *InvalidMethodError) Call(s *slip.Scope, args slip.List, depth int) (cond slip.Object) {
	slip.ArgCountCheck(f, args, 2, -1)
	method, ok := args[0].(slip.Symbol)
	if !ok {
		slip.PanicType("method", args[0], "symbol")
	}
	var ctrl slip.String

	if ctrl, ok = args[1].(slip.String); !ok {
		slip.PanicType("format-control", args[1], "string")
	}
	var sf SimpleFormatterEmbed
	sf.Init(s, string(ctrl), args[2:])

	panic(slip.NewMethodError(nil, nil, method, "%s", sf.Output()))
}
