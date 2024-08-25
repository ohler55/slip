// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"net"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/clos"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SocketState{Function: slip.Function{Name: "socket-state", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-state",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "usocket",
					Text: "to return the state of.",
				},
			},
			Return: "nil|:read-write|:read|:write",
			Text:   `__socket-state__ returns the state of a _usocket_ instance.`,
			Examples: []string{
				`(socket-state (make-instance 'usocket)) => nil`,
			},
		}, &Pkg)
}

// SocketState represents the socket-state function.
type SocketState struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketState) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if ok && self.Any != nil {
		if _, ok = self.Any.(net.Conn); ok {
			result = slip.Symbol(":read-write")
		}
	}
	return
}

type usocketStateCaller struct{}

func (caller usocketStateCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.ArgCountCheck(self, args, 0, 0)
	if self.Any != nil {
		result = slip.Symbol(":read-write")
	}
	return
}

func (caller usocketStateCaller) Docs() string {
	return clos.MethodDocFromFunc(":state", "socket-state", "usocket", "socket")
}
