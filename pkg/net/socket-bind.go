// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"syscall"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSocketBind() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SocketBind{Function: slip.Function{Name: "socket-bind", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-bind",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to bind.",
				},
				{Name: "&rest"},
				{
					Name: "address",
					Type: "string|octets,fixnum",
					Text: `the address can be either a _string_ for a unix socket or an
address and port for an inet socket.`,
				},
			},
			Return: "nil",
			Text:   `__socket-bind__ binds _socket_ to an address.`,
			Examples: []string{
				`(let ((sock (make-socket :domain :unix :type :stream)))`,
				`  (socket-bind sock #(127 0 0 1) 1234)) => nil`,
			},
		}, &Pkg)
}

// SocketBind represents the socket-bind function.
type SocketBind struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketBind) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 3)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA("socket") {
		slip.TypePanic(s, depth, "socket", args[0], "socket")
	}
	bindSocket(s, self, args[1:], depth)
	return nil
}

type socketBindCaller struct{}

func (caller socketBindCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":bind", args, 0, 2)
	bindSocket(s, self, args, depth)
	return nil
}

func (caller socketBindCaller) FuncDocs() *slip.FuncDoc {
	md := methodDocFromFunc(":bind", "socket-bind", &Pkg)
	md.Examples[len(md.Examples)-1] = `  (send sock :bind #(127 0 0 1) 1234)) => nil`
	return md
}

func bindSocket(s *slip.Scope, self *flavors.Instance, args slip.List, depth int) {
	fd, sa := getAddressArgs(s, self, args, depth)
	if err := syscall.Bind(fd, sa); err != nil {
		panic(err)
	}
}
