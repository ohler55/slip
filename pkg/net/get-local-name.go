// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"net"
	"strconv"
	"strings"

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
			Return: "string,fixnum",
			Text: `__get-local-name__ returns the address and port of the _socket_. If the _socket_
is closed then _nil_,_nil_ is returned. A Unix socket has an empty address string and a port of zero.`,
			Examples: []string{
				`(get-local-name (make-instance 'usocket)) => "127.0.0.1", 8080`,
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
		result[0] = slip.String(addr)
		result[1] = slip.Fixnum(port)
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
		result[0] = slip.String(addr)
		result[1] = slip.Fixnum(port)
	}
	return result
}

func (caller usocketLocalNameCaller) Docs() string {
	return clos.MethodDocFromFunc(":local-name", "get-local-name", "usocket", "socket")
}

func usocketLocalName(self *flavors.Instance) (address string, port int) {
	nc, _ := self.Any.(net.Conn)

	return splitAddrString(nc.LocalAddr().String())
}

func splitAddrString(addr string) (address string, port int) {
	// "192.0.2.1:25", "[2001:db8::1]:80"
	pos := strings.LastIndexByte(addr, ':')
	if 0 <= pos {
		address = addr[:pos]
		port, _ = strconv.Atoi(addr[pos+1:])
	}
	return
}
