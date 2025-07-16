// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSocketp() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SocketP{Function: slip.Function{Name: "socket-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-p",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "object",
					Text: "object to check.",
				},
			},
			Return: "boolean",
			Text:   `__socket-p__ returns _t_ if _socket_ is an instance of a _socket_ class and _nil_ otherwise.`,
			Examples: []string{
				`(socket-p (make-instance 'socket)) => t`,
			},
		}, &Pkg)
}

// SocketP represents the socket-p function.
type SocketP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketP) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if self, ok := args[0].(*flavors.Instance); ok && self.IsA("socket") {
		result = slip.True
	}
	return
}
