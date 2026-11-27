// Copyright (c) 2026, Peter Ohler, All rights reserved.

package wos

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defWosp() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := wosp{Function: slip.Function{Name: "wosp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "wosp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: `An object to check.`,
				},
			},
			Return: "wos",
			Text:   `__wosp__ returns true if the _object_ is an instance of the wos-flavor.`,
			Examples: []string{
				`(wosp (make-wos)) => t`,
			},
		}, &Pkg)
}

// wosp represents the wosp-wos function.
type wosp struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *wosp) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)

	if inst, ok := args[0].(*flavors.Instance); ok && inst.Type == flavor {
		result = slip.True
	}
	return
}
