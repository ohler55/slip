// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"syscall"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSocketReceive() {
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
					Type: "socket",
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
				{
					Name: "oob",
					Type: "boolean",
					Text: "if true then the datagram MSG_OOB flag is set.",
				},
				{
					Name: "peek",
					Type: "boolean",
					Text: "if true then the datagram MSG_PEEK flag is set.",
				},
				{
					Name: "waitall",
					Type: "boolean",
					Text: "if true then the datagram MSG_WAITALL flag is set.",
				},
				{
					Name: "dontwait",
					Type: "boolean",
					Text: "if true then the datagram MSG_DONTWAIT flag is set.",
				},
			},
			Return: "octets,fixnum,list",
			Text: `__socket-receive__ reads from the _socket_ and returns the three values;
the buffer, the number of bytes received, and the peer address as a list of host and port.`,
			Examples: []string{
				`(let ((sock (make-instance 'socket :socket 777)))`,
				`  (socket-receive sock)) => #(65 66 67), 3, #(127 0 0 1)`,
			},
		}, &Pkg)
}

// SocketReceive represents the socket-receive function.
type SocketReceive struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketReceive) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 13)
	self, ok := args[0].(*flavors.Instance)
	if ok && self.Any != nil {
		if fd, ok2 := self.Any.(int); ok2 {
			result = socketReceive(s, fd, args[1:], depth)
		}
	}
	return
}

type socketReceiveCaller struct{}

func (caller socketReceiveCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.CheckSendArgCount(s, depth, self, ":receive", args, 1, 8)
	if self.Any != nil {
		result = socketReceive(s, self.Any.(int), args, depth)
	}
	return
}

func (caller socketReceiveCaller) FuncDocs() *slip.FuncDoc {
	md := methodDocFromFunc(":receive", "socket-receive", &Pkg)
	md.Examples[len(md.Examples)-1] = `  (send sock :receive)) => #(65 66 67), 3, #(127 0 0 1)`
	return md
}

func socketReceive(s *slip.Scope, fd int, args slip.List, depth int) slip.Object {
	result := slip.Values{nil, nil, nil}
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
			slip.TypePanic(s, depth, "buffer", ta, "octets")
		}
		length = len(buf)
		args = args[1:]
		if 0 < len(args) {
			if args[0] != nil {
				if num, ok := args[0].(slip.Fixnum); ok {
					if num <= 0 {
						slip.TypePanic(s, depth, "length", num, "positive fixnum")
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
		var (
			rset FdSet
			eset FdSet
		)
		rset.Set(fd)
		eset.Set(fd)
		if err := Select(&rset, nil, &eset, timeout); err != nil || eset.IsSet(fd) {
			slip.ErrorPanic(s, depth, "read error")
		}
		if !rset.IsSet(fd) {
			slip.ErrorPanic(s, depth, "read timed out")
		}
	}
	typ, err := syscall.GetsockoptInt(fd, syscall.SOL_SOCKET, syscall.SO_TYPE)
	if err != nil {
		panic(err)
	}
	if typ == syscall.SOCK_DGRAM {
		out, cnt, addr := datagramReceive(fd, buf, args)
		result[0] = slip.Octets(out)
		result[1] = slip.Fixnum(cnt)
		result[2] = addr
	} else {
		cnt, err := syscall.Read(fd, buf)
		if err != nil || cnt == 0 {
			slip.ErrorPanic(s, depth, "read failed. %s", err)
		}
		result[0] = slip.Octets(buf)
		result[1] = slip.Fixnum(cnt)
		if sa, err := syscall.Getpeername(fd); err == nil {
			addr, port := socketName(sa)
			result[2] = slip.List{addr, port}
		}
	}
	return result
}

func datagramReceive(fd int, buf []byte, args slip.List) ([]byte, int, slip.List) {
	var flags int

	if value, has := slip.GetArgsKeyValue(args, slip.Symbol(":peek")); has && value != nil {
		flags |= syscall.MSG_PEEK
	}
	if value, has := slip.GetArgsKeyValue(args, slip.Symbol(":waitall")); has && value != nil {
		flags |= syscall.MSG_WAITALL
	}
	if value, has := slip.GetArgsKeyValue(args, slip.Symbol(":dontwait")); has && value != nil {
		flags |= syscall.MSG_DONTWAIT
	}
	if value, has := slip.GetArgsKeyValue(args, slip.Symbol(":oob")); has && value != nil {
		flags |= syscall.MSG_OOB
	}
	cnt, sa, err := syscall.Recvfrom(fd, buf, flags)
	if err != nil {
		panic(err)
	}
	var addr slip.List
	switch tsa := sa.(type) {
	case *syscall.SockaddrInet4:
		oct := make(slip.Octets, 4)
		copy(oct, tsa.Addr[:])
		addr = slip.List{oct, slip.Fixnum(tsa.Port)}
	case *syscall.SockaddrInet6:
		oct := make(slip.Octets, 16)
		copy(oct, tsa.Addr[:])
		addr = slip.List{oct, slip.Fixnum(tsa.Port)}
	case *syscall.SockaddrUnix:
		if 0 < len(tsa.Name) {
			addr = slip.List{slip.String(tsa.Name)}
		}
	}
	return buf, cnt, addr
}
