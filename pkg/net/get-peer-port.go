// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := GetPeerPort{Function: slip.Function{Name: "get-peer-port", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "get-peer-port",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "usocket",
					Text: "to get the peer port of.",
				},
			},
			Return: "fixnum",
			Text: `__get-peer-port__ returns the port of the _socket_. If the _socket_
is closed then _nil_ is returned.`,
			Examples: []string{
				`(get-peer-port (make-instance 'usocket)) => 8080`,
			},
		}, &Pkg)
}

// GetPeerPort represents the get-peer-port function.
type GetPeerPort struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *GetPeerPort) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || self.Flavor != usocketFlavor {
		slip.PanicType("socket", args[0], "usocket")
	}
	if self.Any != nil {
		_, port := usocketPeerName(self)
		result = port
	}
	return
}

type usocketPeerPortCaller struct{}

func (caller usocketPeerPortCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":peer-port", args, 0, 0)
	if self.Any != nil {
		_, port := usocketPeerName(self)
		result = port
	}
	return
}
