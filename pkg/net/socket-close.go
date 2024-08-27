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
			f := SocketClose{Function: slip.Function{Name: "socket-close", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-close",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "usocket",
					Text: "to close.",
				},
			},
			Return: "nil|",
			Text:   `__socket-close__ closes the _usocket_ instance.`,
			Examples: []string{
				`(socket-close (make-instance 'usocket)) => nil`,
			},
		}, &Pkg)
}

// SocketClose represents the socket-close function.
type SocketClose struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketClose) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || self.Flavor != usocketFlavor {
		slip.PanicType("socket", args[0], "usocket")
	}
	if nc, ok2 := self.Any.(net.Conn); ok2 {
		_ = nc.Close()
	}
	self.Any = nil

	return nil
}

type usocketCloseCaller struct{}

func (caller usocketCloseCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":close", args, 0, 0)
	if nc, ok2 := self.Any.(net.Conn); ok2 {
		_ = nc.Close()
	}
	self.Any = nil

	return nil
}

func (caller usocketCloseCaller) Docs() string {
	return clos.MethodDocFromFunc(":close", "socket-close", "usocket", "socket")
}
