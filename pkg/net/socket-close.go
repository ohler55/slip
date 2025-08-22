// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"syscall"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSocketClose() {
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
					Type: "socket",
					Text: "to close.",
				},
			},
			Return: "nil",
			Text:   `__socket-close__ closes the _socket_ instance.`,
			Examples: []string{
				`(socket-close (make-instance 'socket)) => nil`,
			},
		}, &Pkg)
}

// SocketClose represents the socket-close function.
type SocketClose struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketClose) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA("socket") {
		slip.TypePanic(s, depth, "socket", args[0], "socket")
	}
	if fd, ok2 := self.Any.(int); ok2 {
		_ = syscall.Close(fd)
	}
	self.Any = nil

	return nil
}

type socketCloseCaller struct{}

func (caller socketCloseCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.CheckSendArgCount(s, depth, self, ":close", args, 0, 0)
	if fd, ok2 := self.Any.(int); ok2 {
		_ = syscall.Close(fd)
	}
	self.Any = nil

	return nil
}

func (caller socketCloseCaller) FuncDocs() *slip.FuncDoc {
	md := methodDocFromFunc(":close", "socket-close", &Pkg)
	md.Examples[0] = `(send (make-instance 'socket) :close) => nil`
	return md
}
