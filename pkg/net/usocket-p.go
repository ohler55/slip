// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := UsocketP{Function: slip.Function{Name: "usocket-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "usocket-p",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "object",
					Text: "object to check.",
				},
			},
			Return: "boolean",
			Text:   `__usocket-p__ returns _t_ if _socket_ is a _usocket_ and _nil_ otherwise.`,
			Examples: []string{
				`(usocket-p (make-instance 'usocket)) => t`,
			},
		}, &Pkg)
}

// UsocketP represents the usocket-p function.
type UsocketP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *UsocketP) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if self, ok := args[0].(*flavors.Instance); ok && self.Flavor == usocketFlavor {
		result = slip.True
	}
	return
}
