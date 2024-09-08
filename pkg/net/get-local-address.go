// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := GetLocalAddress{Function: slip.Function{Name: "get-local-address", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "get-local-address",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "usocket",
					Text: "to get the local address of.",
				},
			},
			Return: "octets|string",
			Text: `__get-local-address__ returns the address of the _socket_. If the _socket_
is closed then _nil_ is returned.`,
			Examples: []string{
				`(get-local-address (make-instance 'usocket :socket 5)) => #(127 0 0 1)`,
			},
		}, &Pkg)
}

// GetLocalAddress represents the get-local-address function.
type GetLocalAddress struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *GetLocalAddress) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || self.Flavor != usocketFlavor {
		slip.PanicType("socket", args[0], "usocket")
	}
	if self.Any != nil {
		addr, _ := usocketLocalName(self)
		result = addr
	}
	return
}

type usocketLocalAddressCaller struct{}

func (caller usocketLocalAddressCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":local-address", args, 0, 0)
	if self.Any != nil {
		addr, _ := usocketLocalName(self)
		result = addr
	}
	return
}
