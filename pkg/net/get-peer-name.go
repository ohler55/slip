// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"net"

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
					Type: "usocket",
					Text: "to get the peer address of.",
				},
			},
			Return: "string,fixnum",
			Text: `__get-peer-name__ returns the address and port of the _socket_. If the _socket_
is closed then _nil_,_nil_ is returned. A Unix socket has an empty address string and a port of zero.`,
			Examples: []string{
				`(get-peer-name (make-instance 'usocket)) => "127.0.0.1", 8080`,
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
	if !ok || self.Flavor != usocketFlavor {
		slip.PanicType("socket", args[0], "usocket")
	}
	result := slip.Values{nil, nil}
	if self.Any != nil {
		addr, port := usocketPeerName(self)
		result[0] = slip.String(addr)
		result[1] = slip.Fixnum(port)
	}
	return result
}

type usocketPeerNameCaller struct{}

func (caller usocketPeerNameCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":peer-name", args, 0, 0)
	result := slip.Values{nil, nil}
	if self.Any != nil {
		addr, port := usocketPeerName(self)
		result[0] = slip.String(addr)
		result[1] = slip.Fixnum(port)
	}
	return result
}

func (caller usocketPeerNameCaller) Docs() string {
	return clos.MethodDocFromFunc(":peer-name", "get-peer-name", "usocket", "socket")
}

func usocketPeerName(self *flavors.Instance) (address string, port int) {
	nc, _ := self.Any.(net.Conn)

	return splitAddrString(nc.RemoteAddr().String())
}
