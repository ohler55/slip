// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"syscall"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSocketName() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SocketName{Function: slip.Function{Name: "socket-name", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-name",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to get the local address of.",
				},
			},
			Return: "octets|string,fixnum",
			Text: `__socket-name__ returns the address as octets and the port of the _socket_.
If the _socket_ is closed then _nil_,_nil_ is returned.`,
			Examples: []string{
				`(let ((sock (make-instance 'socket :socket 5)))`,
				`  (socket-name sock)) => #(127 0 0 1), 1234`,
			},
		}, &Pkg)
}

// SocketName represents the socket-name function.
type SocketName struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketName) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA(socketFlavor) {
		slip.PanicType("socket", args[0], "socket")
	}
	result := slip.Values{nil, nil}
	if self.Any != nil {
		addr, port := socketLocalName(self)
		result[0] = addr
		result[1] = port
	}
	return result
}

type socketNameCaller struct{}

func (caller socketNameCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":name", args, 0, 0)
	result := slip.Values{nil, nil}
	if self.Any != nil {
		addr, port := socketLocalName(self)
		result[0] = addr
		result[1] = port
	}
	return result
}

func (caller socketNameCaller) FuncDocs() *slip.FuncDoc {
	md := methodDocFromFunc(":name", "socket-name", &Pkg)
	md.Examples[len(md.Examples)-1] = `  (send sock :name)) => #(127 0 0 1), 1234`
	return md
}

func socketLocalName(self *flavors.Instance) (address slip.Object, port slip.Fixnum) {
	fd, _ := self.Any.(int)

	sa, err := syscall.Getsockname(fd)
	if err != nil {
		panic(err)
	}
	return socketName(sa)
}

func socketName(sa syscall.Sockaddr) (address slip.Object, port slip.Fixnum) {
	switch tsa := sa.(type) {
	case *syscall.SockaddrUnix:
		if len(tsa.Name) == 0 {
			tsa.Name = "@"
		}
		address = slip.String(tsa.Name)
	case *syscall.SockaddrInet4:
		addr := make(slip.Octets, len(tsa.Addr))
		copy(addr, tsa.Addr[:])
		address = addr
		port = slip.Fixnum(tsa.Port)
	case *syscall.SockaddrInet6:
		addr := make(slip.Octets, len(tsa.Addr))
		copy(addr, tsa.Addr[:])
		address = addr
		port = slip.Fixnum(tsa.Port)
	}
	return
}
