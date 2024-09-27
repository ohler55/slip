// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SocketPeerPort{Function: slip.Function{Name: "socket-peer-port", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-peer-port",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to get the peer port of.",
				},
			},
			Return: "fixnum",
			Text: `__socket-peer-port__ returns the port of the _socket_. If the _socket_
is closed then _nil_ is returned.`,
			Examples: []string{
				`(socket-peer-port (make-instance 'socket)) => 8080`,
			},
		}, &Pkg)
}

// SocketPeerPort represents the socket-peer-port function.
type SocketPeerPort struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketPeerPort) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || self.Flavor != socketFlavor {
		slip.PanicType("socket", args[0], "socket")
	}
	if self.Any != nil {
		_, port := socketPeerName(self)
		result = port
	}
	return
}

type socketPeerPortCaller struct{}

func (caller socketPeerPortCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":peer-port", args, 0, 0)
	if self.Any != nil {
		_, port := socketPeerName(self)
		result = port
	}
	return
}
