// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := GetLocalPort{Function: slip.Function{Name: "get-local-port", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "get-local-port",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "usocket",
					Text: "to get the local port of.",
				},
			},
			Return: "fixnum",
			Text: `__get-local-port__ returns the port of the _socket_. If the _socket_
is closed then _nil_ is returned..`,
			Examples: []string{
				`(get-local-port (make-instance 'usocket :socket 5)) => 8080`,
			},
		}, &Pkg)
}

// GetLocalPort represents the get-local-port function.
type GetLocalPort struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *GetLocalPort) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || self.Flavor != usocketFlavor {
		slip.PanicType("socket", args[0], "usocket")
	}
	if self.Any != nil {
		_, port := usocketLocalName(self)
		result = port
	}
	return
}

type usocketLocalPortCaller struct{}

func (caller usocketLocalPortCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":local-port", args, 0, 0)
	if self.Any != nil {
		_, port := usocketLocalName(self)
		result = port
	}
	return
}
