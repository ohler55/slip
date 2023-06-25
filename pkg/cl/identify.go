// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Identify{Function: slip.Function{Name: "identify", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "identify",
			Args: []*slip.DocArg{
				{Name: "object", Type: "object"},
			},
			Return: "object",
			Text:   `__identify__ returns _object_.`,
			Examples: []string{
				"(identify 4) => 4",
			},
		}, &slip.CLPkg)
}

// Identify represents the identify function.
type Identify struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Identify) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)

	return args[0]
}
