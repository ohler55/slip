// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"syscall"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSocketPeerName() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SocketPeerName{Function: slip.Function{Name: "socket-peer-name", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-peer-name",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to get the peer address of.",
				},
			},
			Return: "octets|string,fixnum",
			Text: `__socket-peer-name__ returns the address as octets and the port of the _socket_.
If the _socket_ is closed then _nil_,_nil_ is returned.`,
			Examples: []string{
				`(let ((sock (make-instance 'socket :socket 5)))`,
				`  (socket-peer-name sock)) => #(127 0 0 1), 1234`,
			},
		}, &Pkg)
}

// SocketName represents the socket-peername function.
type SocketPeerName struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketPeerName) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA("socket") {
		slip.TypePanic(s, depth, "socket", args[0], "socket")
	}
	result := slip.Values{nil, nil}
	if self.Any != nil {
		addr, port := socketPeerName(self)
		result[0] = addr
		result[1] = port
	}
	return result
}

type socketPeerNameCaller struct{}

func (caller socketPeerNameCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.CheckSendArgCount(s, depth, self, ":peer-name", args, 0, 0)
	result := slip.Values{nil, nil}
	if self.Any != nil {
		addr, port := socketPeerName(self)
		result[0] = addr
		result[1] = port
	}
	return result
}

func (caller socketPeerNameCaller) FuncDocs() *slip.FuncDoc {
	md := methodDocFromFunc(":peer-name", "socket-peer-name", &Pkg)
	md.Examples[len(md.Examples)-1] = `  (send sock :peer-name)) => #(127 0 0 1), 1234`
	return md
}

func socketPeerName(self *flavors.Instance) (address slip.Object, port slip.Fixnum) {
	fd, _ := self.Any.(int)

	sa, err := syscall.Getpeername(fd)
	if err != nil {
		panic(err)
	}
	return socketName(sa)
}
