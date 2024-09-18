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
			f := GetPeerName{Function: slip.Function{Name: "get-peer-name", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "get-peer-name",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to get the peer address of.",
				},
			},
			Return: "octets|string,fixnum",
			Text: `__get-local-name__ returns the address as octets and the port of the _socket_.
If the _socket_ is closed then _nil_,_nil_ is returned.`,
			Examples: []string{
				`(get-peer-name (make-instance 'socket :socket 5)) => #(127 0 0 1), 1234`,
			},
		}, &Pkg)
}

// GetPeerName represents the get-peer-name function.
type GetPeerName struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *GetPeerName) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || self.Flavor != socketFlavor {
		slip.PanicType("socket", args[0], "socket")
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

func (caller socketPeerNameCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":peer-name", args, 0, 0)
	result := slip.Values{nil, nil}
	if self.Any != nil {
		addr, port := socketPeerName(self)
		result[0] = addr
		result[1] = port
	}
	return result
}

func (caller socketPeerNameCaller) Docs() string {
	return clos.MethodDocFromFunc(":peer-name", "get-peer-name", "socket", "socket")
}

func socketPeerName(self *flavors.Instance) (address slip.Object, port slip.Fixnum) {
	fd, _ := self.Any.(int)

	sa, err := syscall.Getpeername(fd)
	if err != nil {
		panic(err)
	}
	return socketName(sa)
}
