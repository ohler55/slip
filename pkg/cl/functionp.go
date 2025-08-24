// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Functionp{Function: slip.Function{Name: "functionp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "functionp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "t|nil",
			Text:   `__functionp__ returns _true_ if _object_ is a function or lambda.`,
			Examples: []string{
				`(functionp "abc") => nil`,
				"(functionp #'car) => t",
			},
		}, &slip.CLPkg)
}

// Functionp represents the functionp function.
type Functionp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Functionp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	switch args[0].(type) {
	case *slip.Lambda, *slip.Function, *slip.FuncInfo:
		return slip.True
	}
	return nil
}
