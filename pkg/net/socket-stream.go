// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/clos"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SocketStream{Function: slip.Function{Name: "socket-stream", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-stream",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to stream.",
				},
			},
			Return: "nil|",
			Text:   `__socket-stream__ returns a stream of the _socket_ instance.`,
			Examples: []string{
				`(socket-stream (make-instance 'socket)) => #<IO-STREAM>`,
			},
		}, &Pkg)
}

// SocketStream represents the socket-stream function.
type SocketStream struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketStream) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || self.Flavor != socketFlavor {
		slip.PanicType("socket", args[0], "socket")
	}
	if fd, ok2 := self.Any.(int); ok2 {
		return &slip.IOStream{RW: fdRW(fd)}
	}
	return nil
}

type socketStreamCaller struct{}

func (caller socketStreamCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":stream", args, 0, 0)
	if fd, ok2 := self.Any.(int); ok2 {
		return &slip.IOStream{RW: fdRW(fd)}
	}
	return nil
}

func (caller socketStreamCaller) Docs() string {
	return clos.MethodDocFromFunc(":stream", "socket-stream", "socket", "socket")
}
