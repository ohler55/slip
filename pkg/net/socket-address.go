// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SocketAddress{Function: slip.Function{Name: "socket-address", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-address",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to get the local address of.",
				},
			},
			Return: "octets|string",
			Text: `__socket-address__ returns the address of the _socket_. If the _socket_
is closed then _nil_ is returned.`,
			Examples: []string{
				`(socket-address (make-instance 'socket :socket 5)) => #(127 0 0 1)`,
			},
		}, &Pkg)
}

// SocketAddress represents the socket-address function.
type SocketAddress struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketAddress) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA(socketFlavor) {
		slip.PanicType("socket", args[0], "socket")
	}
	if self.Any != nil {
		addr, _ := socketLocalName(self)
		result = addr
	}
	return
}

type socketAddressCaller struct{}

func (caller socketAddressCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":address", args, 0, 0)
	if self.Any != nil {
		addr, _ := socketLocalName(self)
		result = addr
	}
	return
}
