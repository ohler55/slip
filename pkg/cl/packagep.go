// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Packagep{Function: slip.Function{Name: "packagep", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "packagep",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "Object check.",
				},
			},
			Return: "boolean",
			Text:   `__packagep__ returns true if _object_ is a _package_.`,
			Examples: []string{
				`(packagep :cl) => t`,
				`(packagep t) => nil`,
			},
		}, &slip.CLPkg)
}

// Packagep represents the packagep function.
type Packagep struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Packagep) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	if _, ok := args[0].(*slip.Package); ok {
		return slip.True
	}
	return nil
}
