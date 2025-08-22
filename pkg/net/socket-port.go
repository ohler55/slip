// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSocketPort() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SocketPort{Function: slip.Function{Name: "socket-port", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-port",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to get the local port of.",
				},
			},
			Return: "fixnum",
			Text: `__socket-port__ returns the port of the _socket_. If the _socket_
is closed then _nil_ is returned..`,
			Examples: []string{
				`(let ((sock (make-instance 'socket :socket 5)))`,
				`  (socket-port sock)) => 8080`,
			},
		}, &Pkg)
}

// SocketPort represents the socket-port function.
type SocketPort struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketPort) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA("socket") {
		slip.TypePanic(s, depth, "socket", args[0], "socket")
	}
	if self.Any != nil {
		_, port := socketLocalName(self)
		result = port
	}
	return
}

type socketPortCaller struct{}

func (caller socketPortCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":port", args, 0, 0)
	if self.Any != nil {
		_, port := socketLocalName(self)
		result = port
	}
	return
}

func (caller socketPortCaller) FuncDocs() *slip.FuncDoc {
	md := methodDocFromFunc(":port", "socket-port", &Pkg)
	md.Examples[len(md.Examples)-1] = `  (send sock :port)) => 8080`
	return md
}
