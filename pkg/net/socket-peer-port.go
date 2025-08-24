// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSocketPeerPort() {
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
				`(let ((sock (make-instance 'socket)))`,
				`  (socket-peer-port sock)) => 8080`,
			},
		}, &Pkg)
}

// SocketPeerPort represents the socket-peer-port function.
type SocketPeerPort struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketPeerPort) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA("socket") {
		slip.TypePanic(s, depth, "socket", args[0], "socket")
	}
	if self.Any != nil {
		_, port := socketPeerName(self)
		result = port
	}
	return
}

type socketPeerPortCaller struct{}

func (caller socketPeerPortCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.CheckSendArgCount(s, depth, self, ":peer-port", args, 0, 0)
	if self.Any != nil {
		_, port := socketPeerName(self)
		result = port
	}
	return
}

func (caller socketPeerPortCaller) FuncDocs() *slip.FuncDoc {
	md := methodDocFromFunc(":peer-port", "socket-peer-port", &Pkg)
	md.Examples[len(md.Examples)-1] = `  (send sock :peer-port)) => 8080`
	return md
}
