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
			f := SocketConnect{Function: slip.Function{Name: "socket-connect", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-connect",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to connect.",
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
			Text:   `__socket-connect__ connects _socket_ to an address.`,
			Examples: []string{
				`(let ((sock (make-socket :domain :unix :type :stream)))`,
				` (socket-connect sock #(127 0 0 1) 1234)) => nil`,
			},
		}, &Pkg)
}

// SocketConnect represents the socket-connect function.
type SocketConnect struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketConnect) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 3)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA(socketFlavor) {
		slip.PanicType("socket", args[0], "socket")
	}
	connectSocket(self, args[1:])
	return nil
}

type socketConnectCaller struct{}

func (caller socketConnectCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":connect", args, 0, 2)
	connectSocket(self, args)
	return nil
}

func (caller socketConnectCaller) Docs() string {
	return clos.MethodDocFromFunc(":connect", "socket-connect", "socket", "socket")
}

func connectSocket(self *flavors.Instance, args slip.List) {
	fd, sa := getAddressArgs(self, args)
	if err := syscall.Connect(fd, sa); err != nil {
		panic(err)
	}
}

func getAddressArgs(self *flavors.Instance, args slip.List) (fd int, sa syscall.Sockaddr) {
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
	var ok bool
	if fd, ok = self.Any.(int); !ok {
		slip.NewPanic("%s is not initialized", self)
	}
	return
}
