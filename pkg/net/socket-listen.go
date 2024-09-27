// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"syscall"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/clos"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SocketListen{Function: slip.Function{Name: "socket-listen", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-listen",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to listen.",
				},
				{
					Name: "backlog",
					Type: "fixnum",
					Text: `the maximum connection backlog.`,
				},
			},
			Return: "nil",
			Text:   `__socket-listen__ listens on _socket_ for connection requests.`,
			Examples: []string{
				`(let ((sock (make-socket :domain :unix :type :stream)))`,
				` (socket-bind sock #(127 0 0 1) 1234)`,
				` (socket-listen sock 1000))`,
			},
		}, &Pkg)
}

// SocketListen represents the socket-listen function.
type SocketListen struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketListen) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA(socketFlavor) {
		slip.PanicType("socket", args[0], "socket")
	}
	listenSocket(self, args[1:])
	return nil
}

type socketListenCaller struct{}

func (caller socketListenCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":listen", args, 1, 1)
	listenSocket(self, args)
	return nil
}

func (caller socketListenCaller) Docs() string {
	return clos.MethodDocFromFunc(":listen", "socket-listen", "socket", "socket")
}

func listenSocket(self *flavors.Instance, args slip.List) {
	backlog, ok := args[0].(slip.Fixnum)
	if !ok {
		slip.PanicType("backlog", args[0], "fixnum")
	}
	var fd int
	if fd, ok = self.Any.(int); !ok {
		slip.NewPanic("%s is not initialized", self)
	}
	if err := syscall.Listen(fd, int(backlog)); err != nil {
		panic(err)
	}
}
