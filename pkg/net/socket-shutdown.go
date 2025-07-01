// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"syscall"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSocketShutdown() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SocketShutdown{Function: slip.Function{Name: "socket-shutdown", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-shutdown",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to shutdown.",
				},
				{Name: "&key"},
				{
					Name: "direction",
					Type: "keyword",
					Text: "the direction to shutdown. Can be _:input_, _:output_, or _:io_.",
				},
			},
			Return: "nil",
			Text: `__socket-shutdown__ shuts down the specified direction on the _socket_ instance.
If no direction is specified then no change is made to the _socket_. On some platforms such as linux
the shutdown will not actually block the reading or writing.`,
			Examples: []string{
				`(let ((sock (make-instance 'socket :socket 5)))`,
				`  (socket-shutdown sock :direction :input)) => nil`,
			},
		}, &Pkg)
}

// SocketShutdown represents the socket-shutdown function.
type SocketShutdown struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketShutdown) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 3)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA(socketFlavor) {
		slip.PanicType("socket", args[0], "socket")
	}
	shutdownSocket(self, args[1:])
	return nil
}

type socketShutdownCaller struct{}

func (caller socketShutdownCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":shutdown", args, 0, 2)
	shutdownSocket(self, args)
	return nil
}

func (caller socketShutdownCaller) FuncDocs() *slip.FuncDoc {
	md := methodDocFromFunc(":shutdown", "socket-shutdown", &Pkg)
	md.Examples[len(md.Examples)-1] = `  (socket-shutdown sock :direction :input)) => nil`
	return md
}

func shutdownSocket(self *flavors.Instance, args slip.List) {
	if fd, ok := self.Any.(int); ok {
		var err error
		if value, has := slip.GetArgsKeyValue(args, slip.Symbol(":direction")); has {
			switch value {
			case slip.Symbol(":input"):
				err = syscall.Shutdown(fd, syscall.SHUT_RD)
			case slip.Symbol(":output"):
				err = syscall.Shutdown(fd, syscall.SHUT_WR)
			case slip.Symbol(":io"):
				err = syscall.Shutdown(fd, syscall.SHUT_RDWR)
			default:
				slip.PanicType(":direction", value, ":input", ":output", ":io")
			}
		}
		if err != nil {
			panic(err)
		}
	}
}
