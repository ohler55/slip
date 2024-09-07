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
			f := SocketReceive{Function: slip.Function{Name: "socket-receive", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-receive",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "usocket",
					Text: "to receive a data on.",
				},
				{Name: "&optional"},
				{
					Name: "buffer",
					Type: "octets",
					Text: "the vector (octets) to place the received data in.",
				},
				{
					Name: "length",
					Type: "fixnum",
					Text: "the number of octets to receive. If _nil_ then the length of _buffer_ used.",
				},
				{Name: "&key"},
				{
					Name: "timeout",
					Type: "real|nil",
					Text: "if non-nil then the number of seconds for the timeout.",
				},
			},
			Return: "fixnum",
			Text:   `__socket-receive__ reads from the _socket_ and returns the number of bytes written.`,
			Examples: []string{
				`(socket-receive (make-instance 'usocket :socket 777) "hello" 5) => 5`,
			},
		}, &Pkg)
}

// SocketReceive represents the socket-receive function.
type SocketReceive struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketReceive) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 5)
	self, ok := args[0].(*flavors.Instance)
	if ok && self.Any != nil {
		if nc, ok2 := self.Any.(net.Conn); ok2 {
			result = socketReceive(nc, args[1:])
		}
	}
	return
}

type usocketReceiveCaller struct{}

func (caller usocketReceiveCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":receive", args, 1, 8)
	if self.Any != nil {
		result = socketReceive(self.Any.(net.Conn), args)
	}
	return
}

func (caller usocketReceiveCaller) Docs() string {
	return clos.MethodDocFromFunc(":receive", "socket-receive", "usocket", "socket")
}

func socketReceive(nc net.Conn, args slip.List) slip.Object {
	result := slip.Values{nil, nil, nil, nil}
	var (
		buf    []byte
		length int
	)
	timeout := time.Duration(0)
	if 0 < len(args) {
		switch ta := args[0].(type) {
		case nil:
			// leave buf as nil or empty
		case slip.Octets:
			buf = []byte(ta)
		default:
			slip.PanicType("buffer", ta, "octets")
		}
		length = len(buf)
		args = args[1:]
		if 0 < len(args) {
			if args[0] != nil {
				if num, ok := args[0].(slip.Fixnum); ok {
					if num <= 0 {
						slip.PanicType("length", num, "positive fixnum")
					}
					length = int(num)
					args = args[1:]
				}
			}
		}
		if value, has := slip.GetArgsKeyValue(args, slip.Symbol(":timeout")); has && value != nil {
			if r, ok := value.(slip.Real); ok {
				if sec := r.RealValue() * float64(time.Second); 0.0 < sec {
					timeout = time.Duration(sec)
				}
			}
		}
	}
	if len(buf) == 0 {
		if length <= 0 {
			length = 65507
		}
		buf = make([]byte, length)
	}
	if 0 < timeout {
		var zero time.Time
		_ = nc.SetReadDeadline(time.Now().Add(timeout))
		defer func() { _ = nc.SetReadDeadline(zero) }()
	}
	cnt, err := nc.Read(buf)
	if err != nil {
		panic(err)
	}
	result[0] = slip.Octets(buf)
	result[1] = slip.Fixnum(cnt)
	addr, port := splitAddrString(nc.RemoteAddr().String())
	result[2] = slip.String(addr)
	result[3] = slip.Fixnum(port)

	return result
}
