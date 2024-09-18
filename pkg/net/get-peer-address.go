// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := GetPeerAddress{Function: slip.Function{Name: "get-peer-address", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "get-peer-address",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to get the peer address of.",
				},
			},
			Return: "octets|string",
			Text: `__get-peer-address__ returns the address of the _socket_. If the _socket_
is closed then _nil_ is returned.`,
			Examples: []string{
				`(get-peer-address (make-instance 'socket :socket 5)) => #(127 0 0 1)`,
			},
		}, &Pkg)
}

// GetPeerAddress represents the get-peer-address function.
type GetPeerAddress struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *GetPeerAddress) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || self.Flavor != socketFlavor {
		slip.PanicType("socket", args[0], "socket")
	}
	if self.Any != nil {
		addr, _ := socketPeerName(self)
		result = addr
	}
	return
}

type socketPeerAddressCaller struct{}

func (caller socketPeerAddressCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":peer-address", args, 0, 0)
	if self.Any != nil {
		addr, _ := socketPeerName(self)
		result = addr
	}
	return
}
