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
			f := GetLocalName{Function: slip.Function{Name: "get-local-name", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "get-local-name",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "usocket",
					Text: "to get the local address of.",
				},
			},
			Return: "octets|string,fixnum",
			Text: `__get-local-name__ returns the address as octets and the port of the _socket_.
If the _socket_ is closed then _nil_,_nil_ is returned.`,
			Examples: []string{
				`(get-local-name (make-instance 'usocket :socket 5)) => #(127 0 0 1), 1234`,
			},
		}, &Pkg)
}

// GetLocalName represents the get-local-name function.
type GetLocalName struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *GetLocalName) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || self.Flavor != usocketFlavor {
		slip.PanicType("socket", args[0], "usocket")
	}
	result := slip.Values{nil, nil}
	if self.Any != nil {
		addr, port := usocketLocalName(self)
		result[0] = addr
		result[1] = port
	}
	return result
}

type usocketLocalNameCaller struct{}

func (caller usocketLocalNameCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":local-name", args, 0, 0)
	result := slip.Values{nil, nil}
	if self.Any != nil {
		addr, port := usocketLocalName(self)
		result[0] = addr
		result[1] = port
	}
	return result
}

func (caller usocketLocalNameCaller) Docs() string {
	return clos.MethodDocFromFunc(":local-name", "get-local-name", "usocket", "socket")
}

func usocketLocalName(self *flavors.Instance) (address slip.Object, port slip.Fixnum) {
	fd, _ := self.Any.(int)

	sa, err := syscall.Getsockname(fd)
	if err != nil {
		panic(err)
	}
	return usocketName(sa)
}

func usocketName(sa syscall.Sockaddr) (address slip.Object, port slip.Fixnum) {
	switch tsa := sa.(type) {
	case *syscall.SockaddrUnix:
		if len(tsa.Name) == 0 {
			tsa.Name = "@"
		}
		address = slip.String(tsa.Name)
	case *syscall.SockaddrInet4:
		addr := make(slip.Octets, len(tsa.Addr))
		for i, b := range tsa.Addr {
			addr[i] = b
		}
		address = addr
		port = slip.Fixnum(tsa.Port)
	case *syscall.SockaddrInet6:
		addr := make(slip.Octets, len(tsa.Addr))
		for i, b := range tsa.Addr {
			addr[i] = b
		}
		address = addr
		port = slip.Fixnum(tsa.Port)
	}
	return
}
