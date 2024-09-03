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
			f := SocketSend{Function: slip.Function{Name: "socket-send", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-send",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "usocket",
					Text: "to send a data on.",
				},
				{
					Name: "buffer",
					Type: "octets",
					Text: "the data to send.",
				},
				{Name: "&optional"},
				{
					Name: "length",
					Type: "fixnum",
					Text: "the number of octets to send. If _nil_ then all of the _buffer_ is sent.",
				},
				{Name: "&key"},
				{
					Name: "offset",
					Type: "fixnum",
					Text: "the offset in the _buffer_ to start sending.",
				},
				{
					Name: "host",
					Type: "string|fixnum|list",
					Text: "the host to send to if a datagram socket.",
				},
				{
					Name: "port",
					Type: "fixnum",
					Text: "the port to send to if a datagram socket.",
				},
			},
			Return: "fixnum",
			Text:   `__socket-send__ writes to the _socket_ and returns the number of bytes written.`,
			Examples: []string{
				`(socket-send (make-instance 'usocket :socket 777) "hello" 5) => 5`,
			},
		}, &Pkg)
}

// SocketSend represents the socket-send function.
type SocketSend struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketSend) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 9)
	self, ok := args[0].(*flavors.Instance)
	if ok && self.Any != nil {
		if nc, ok2 := self.Any.(net.Conn); ok2 {
			result = slip.Fixnum(socketSend(nc, args[1:]))
		}
	}
	return
}

type usocketSendCaller struct{}

func (caller usocketSendCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":send", args, 1, 8)
	if self.Any != nil {
		result = slip.Fixnum(socketSend(self.Any.(net.Conn), args))
	}
	return
}

func (caller usocketSendCaller) Docs() string {
	return clos.MethodDocFromFunc(":send", "socket-send", "usocket", "socket")
}

func socketSend(nc net.Conn, args slip.List) int {
	var buf []byte
	switch ta := args[0].(type) {
	case slip.Octets:
		buf = []byte(ta)
	case slip.String:
		buf = []byte(ta)
	default:
		slip.PanicType("buffer", ta, "octets", "string")
	}
	length := len(buf)

	// TBD if 1 < len(args)
	//   if args[1] is a fixnum then length and trim args
	//
	// TBD get buffer, length offset,
	if length < len(buf) {
		buf = buf[:length]
	}
	cnt, err := nc.Write(buf)
	if err != nil {
		panic(err)
	}
	return cnt
}
