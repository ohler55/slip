// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"net/netip"
	"syscall"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/pkg/clos"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
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
				` (socket-bind sock #(127 0 0 1) 1234)) => nil`,
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
	if !ok || !self.IsA(socketFlavor) {
		slip.PanicType("socket", args[0], "socket")
	}
	bindSocket(self, args[1:])
	return nil
}

type socketBindCaller struct{}

func (caller socketBindCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":bind", args, 0, 2)
	bindSocket(self, args)
	return nil
}

func (caller socketBindCaller) Docs() string {
	return clos.MethodDocFromFunc(":bind", "socket-bind", "socket", "socket")
}

func bindSocket(self *flavors.Instance, args slip.List) {
	var sa syscall.Sockaddr
	switch len(args) {
	case 1:
		if ss, ok := args[0].(slip.String); ok {
			sa = &syscall.SockaddrUnix{Name: string(ss)}
		} else {
			slip.PanicType("address", args, "string", "octets,fixnum")
		}
	case 2:
		var (
			addr []byte
			port int
		)
		switch ta := args[0].(type) {
		case slip.String:
			addr = netip.MustParseAddr(string(ta)).AsSlice()
		case *slip.Vector:
			list := ta.AsList()
			octs := make([]byte, len(list))
			for i, r := range list {
				octs[i] = byte(cl.ToOctet(r).(slip.Octet))
			}
			addr = octs
		case slip.Octets:
			addr = []byte(ta)
		default:
			slip.PanicType("address", args, "string", "octets,fixnum")
		}
		if num, ok := args[1].(slip.Fixnum); ok {
			port = int(num)
		} else {
			slip.PanicType("address port", args[1], "fixnum")
		}
		switch len(addr) {
		case 4:
			sa = &syscall.SockaddrInet4{Port: port, Addr: [4]byte(addr)}
		case 16:
			sa = &syscall.SockaddrInet6{Port: port, Addr: [16]byte(addr)}
		default:
			slip.PanicType("address", args, "string", "octets,fixnum")
		}
	default:
		slip.PanicType("address", args, "string", "octets,fixnum")
	}

	fd, ok := self.Any.(int)
	if !ok {
		slip.NewPanic("%s is not initialized", self)
	}
	if err := syscall.Bind(fd, sa); err != nil {
		panic(err)
	}
}
