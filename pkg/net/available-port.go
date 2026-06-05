// Copyright (c) 2026, Peter Ohler, All rights reserved.

package net

import (
	"net"

	"github.com/ohler55/slip"
)

func defAvailablePort() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := AvailablePort{Function: slip.Function{Name: "available-port", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "available-port",
			Args:   []*slip.DocArg{},
			Return: "fixnum",
			Text: `__available-port__ returns an unused port available. If no available
port is found zero is returned`,
			Examples: []string{
				`(available-port) => 12345`,
			},
		}, &Pkg)
}

// AvailablePort represents the available-port function.
type AvailablePort struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *AvailablePort) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 0, 0)

	if addr, err := net.ResolveTCPAddr("tcp", "localhost:0"); err == nil {
		var listener *net.TCPListener
		if listener, err = net.ListenTCP("tcp", addr); err == nil {
			defer func() { _ = listener.Close() }()
			result = slip.Fixnum(listener.Addr().(*net.TCPAddr).Port)
		}
	}
	return
}
