// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"syscall"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSocketListen() {
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
				`  (socket-bind sock #(127 0 0 1) 1234)`,
				`  (socket-listen sock 1000))`,
			},
		}, &Pkg)
}

// SocketListen represents the socket-listen function.
type SocketListen struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketListen) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA("socket") {
		slip.TypePanic(s, depth, "socket", args[0], "socket")
	}
	listenSocket(s, self, args[1:], depth)
	return nil
}

type socketListenCaller struct{}

func (caller socketListenCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.CheckSendArgCount(s, depth, self, ":listen", args, 1, 1)
	listenSocket(s, self, args, depth)
	return nil
}

func (caller socketListenCaller) FuncDocs() *slip.FuncDoc {
	md := methodDocFromFunc(":listen", "socket-listen", &Pkg)
	md.Examples[len(md.Examples)-1] = `  (send sock :listen))`
	return md
}

func listenSocket(s *slip.Scope, self *flavors.Instance, args slip.List, depth int) {
	backlog, ok := args[0].(slip.Fixnum)
	if !ok {
		slip.TypePanic(s, depth, "backlog", args[0], "fixnum")
	}
	var fd int
	if fd, ok = self.Any.(int); !ok {
		slip.NewPanic("%s is not initialized", self)
	}
	if err := syscall.Listen(fd, int(backlog)); err != nil {
		panic(err)
	}
}
