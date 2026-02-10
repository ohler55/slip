// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"syscall"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSocketAccept() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SocketAccept{Function: slip.Function{Name: "socket-accept", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-accept",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to accept a connection on.",
				},
			},
			Return: "socket",
			Text: `__socket-accept__ accepts a connection on the _socket_ instance and
returns a new socket.`,
			Examples: []string{
				`(let ((sock (make-socket :domain :unix :type :stream)))`,
				`  (socket-bind sock #(127 0 0 1) 1234)`,
				`  (socket-listen sock 1000)`,
				`  (socket-accept sock)) => #<socket 1234>`,
			},
		}, &Pkg)
}

// SocketAccept represents the socket-accept function.
type SocketAccept struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketAccept) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA("socket") {
		slip.TypePanic(s, depth, "socket", args[0], "socket")
	}
	return socketAccept(s, self, depth)
}

type socketAcceptCaller struct{}

func (caller socketAcceptCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.CheckSendArgCount(s, depth, self, ":accept", args, 0, 0)

	return socketAccept(s, self, depth)
}

func (caller socketAcceptCaller) FuncDocs() *slip.FuncDoc {
	md := methodDocFromFunc(":accept", "socket-accept", &Pkg)
	md.Examples[len(md.Examples)-1] = `  (send sock :accept)) => #<socket 1234>`
	return md
}

func socketAccept(s *slip.Scope, self *flavors.Instance, depth int) slip.Object {
	fd, ok := self.Any.(int)
	if !ok {
		slip.ErrorPanic(s, depth, "%s is not connected", self)
	}
	nfd, _, err := syscall.Accept(fd)
	if err != nil {
		panic(err)
	}
	syscall.CloseOnExec(nfd)
	sock := socketFlavor.MakeInstance().(*flavors.Instance)
	sock.Any = nfd

	return sock
}
