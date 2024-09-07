// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"net"
	"time"

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
					Name: "timeout",
					Type: "real|nil",
					Text: "if non-nil then the number of seconds for the timeout.",
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
			Return: "octets, fixnum, string, fixnum",
			Text: `__socket-send__ writes to the _socket_ and returns four values;
the octets read, the number of bytes read, the sending host, and the sending port. If _buffer_ is _nil_
then _octets_ or _length_ is created. If both _buffer_ and _length_ are _nil_ then an _octests_ vector
of length 65507 is created.`,
			Examples: []string{
				`(socket-receive (make-instance 'usocket :socket 777) nil 5) => #(65 66 67), 3, "", nil`,
			},
		}, &Pkg)
}

// SocketSend represents the socket-send function.
type SocketSend struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketSend) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 11)
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
	timeout := time.Duration(0)
	if 1 < len(args) {
		args = args[1:]
		if num, ok := args[0].(slip.Fixnum); ok {
			if num <= 0 {
				slip.PanicType("length", num, "positive fixnum")
			}
			length = int(num)
			args = args[1:]
		}
		if value, has := slip.GetArgsKeyValue(args, slip.Symbol(":offset")); has {
			if num, ok := value.(slip.Fixnum); ok && 0 <= num && int(num) < len(buf) {
				buf = buf[int(num):]
			} else {
				slip.NewPanic(":offset must be a positive fixnum less than the length of %d", len(buf))
			}
		}
		if value, has := slip.GetArgsKeyValue(args, slip.Symbol(":timeout")); has && value != nil {
			if r, ok := value.(slip.Real); ok {
				if sec := r.RealValue() * float64(time.Second); 0.0 < sec {
					timeout = time.Duration(sec)
				}
			}
		}
		// TBD host, port for datagrams
	}
	if length < len(buf) {
		buf = buf[:length]
	}
	if 0 < timeout {
		var zero time.Time
		_ = nc.SetWriteDeadline(time.Now().Add(timeout))
		defer func() { _ = nc.SetWriteDeadline(zero) }()
	}
	cnt, err := nc.Write(buf)
	if err != nil {
		panic(err)
	}
	return cnt
}
