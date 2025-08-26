// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"runtime"
	"syscall"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSocketSend() {
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
					Type: "socket",
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
					Text: `if non-nil then the number of seconds for the timeout before at least
one byte can be written.`,
				},
				{
					Name: "address",
					Type: "list",
					Text: "the host and port to send to if a datagram socket.",
				},
				{
					Name: "oob",
					Type: "boolean",
					Text: "if true then the datagram MSG_OOB flag is set.",
				},
				{
					Name: "eor",
					Type: "boolean",
					Text: "if true then the datagram MSG_EOR flag is set.",
				},
				{
					Name: "dontwait",
					Type: "boolean",
					Text: "if true then the datagram MSG_DONTWAIT flag is set.",
				},
				{
					Name: "dontroute",
					Type: "boolean",
					Text: "if true then the datagram MSG_DONTROUTE flag is set.",
				},
				{
					Name: "nosignal",
					Type: "boolean",
					Text: "if true then the datagram MSG_NOSIGNAL flag is set.",
				},
				{
					Name: "confirm",
					Type: "boolean",
					Text: "if true then the datagram MSG_CONFIRM flag is set.",
				},
			},
			Return: "fixnum",
			Text:   `__socket-send__ writes to the _socket_ and returns the number of bytes written.`,
			Examples: []string{
				`(let ((sock (make-instance 'socket :socket 777)))`,
				`  (socket-receive sock "hello")) => 5`,
			},
		}, &Pkg)
}

// SocketSend represents the socket-send function.
type SocketSend struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketSend) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 22)
	self, ok := args[0].(*flavors.Instance)
	if ok && self.Any != nil {
		if fd, ok2 := self.Any.(int); ok2 {
			result = slip.Fixnum(socketSend(s, fd, args[1:], depth))
		}
	}
	return
}

type socketSendCaller struct{}

func (caller socketSendCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.CheckSendArgCount(s, depth, self, ":send", args, 1, 21)
	if self.Any != nil {
		result = slip.Fixnum(socketSend(s, self.Any.(int), args, depth))
	}
	return
}

func (caller socketSendCaller) FuncDocs() *slip.FuncDoc {
	md := methodDocFromFunc(":send", "socket-send", &Pkg)
	md.Examples[len(md.Examples)-1] = `  (send sock :send "hello")) => 5`
	return md
}

func socketSend(s *slip.Scope, fd int, args slip.List, depth int) int {
	var buf []byte
	switch ta := args[0].(type) {
	case slip.Octets:
		buf = []byte(ta)
	case slip.String:
		buf = []byte(ta)
	default:
		slip.TypePanic(s, depth, "buffer", ta, "octets", "string")
	}
	length := len(buf)
	timeout := time.Duration(0)
	args = args[1:]
	if 0 < len(args) {
		if num, ok := args[0].(slip.Fixnum); ok {
			if num <= 0 {
				slip.TypePanic(s, depth, "length", num, "positive fixnum")
			}
			length = int(num)
			args = args[1:]
		}
		if value, has := slip.GetArgsKeyValue(args, slip.Symbol(":offset")); has {
			if num, ok := value.(slip.Fixnum); ok && 0 <= num && int(num) < len(buf) {
				buf = buf[int(num):]
			} else {
				slip.ErrorPanic(s, depth, ":offset must be a positive fixnum less than the length of %d", len(buf))
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
	if length < len(buf) {
		buf = buf[:length]
	}
	if 0 < timeout {
		// Use syscall.Select as it is implemented on more platforms (darwin).
		var (
			wset FdSet
			eset FdSet
		)
		wset.Set(fd)
		eset.Set(fd)
		if err := Select(nil, &wset, &eset, timeout); err != nil || eset.IsSet(fd) {
			slip.ErrorPanic(s, depth, "write error")
		}
		if !wset.IsSet(fd) {
			slip.ErrorPanic(s, depth, "write timed out")
		}
	}
	typ, err := syscall.GetsockoptInt(fd, syscall.SOL_SOCKET, syscall.SO_TYPE)
	if err != nil {
		panic(err)
	}
	var cnt int
	if typ == syscall.SOCK_DGRAM {
		cnt = datagramSend(s, fd, buf, args, depth)
	} else {
		cnt, err = syscall.Write(fd, buf)
		if err != nil {
			panic(err)
		}
	}
	return cnt
}

func datagramSend(s *slip.Scope, fd int, buf []byte, args slip.List, depth int) int {
	var flags int

	if value, has := slip.GetArgsKeyValue(args, slip.Symbol(":oob")); has && value != nil {
		flags |= syscall.MSG_OOB
	}
	if value, has := slip.GetArgsKeyValue(args, slip.Symbol(":eor")); has && value != nil {
		flags |= syscall.MSG_EOR
	}
	if value, has := slip.GetArgsKeyValue(args, slip.Symbol(":dontwait")); has && value != nil {
		flags |= syscall.MSG_DONTWAIT
	}
	if value, has := slip.GetArgsKeyValue(args, slip.Symbol(":dontroute")); has && value != nil {
		flags |= syscall.MSG_DONTROUTE
	}
	if value, has := slip.GetArgsKeyValue(args, slip.Symbol(":nosignal")); has && value != nil {
		if flag, has := msgFlagMap[slip.Symbol(":nosignal")]; has {
			flags |= flag
		} else {
			slip.ErrorPanic(s, depth, ":nosignal flag not support on %s", runtime.GOOS)
		}
	}
	if value, has := slip.GetArgsKeyValue(args, slip.Symbol(":confirm")); has && value != nil {
		if flag, has := msgFlagMap[slip.Symbol(":confirm")]; has {
			flags |= flag
		} else {
			slip.ErrorPanic(s, depth, ":confirm flag not support on %s", runtime.GOOS)
		}
	}
	sa, _ := syscall.Getpeername(fd)
	if value, has := slip.GetArgsKeyValue(args, slip.Symbol(":address")); has {
		switch tv := value.(type) {
		case slip.String:
			sa = &syscall.SockaddrUnix{Name: string(tv)}
		case slip.List:
			sa = addressFromList(s, tv, depth)
		default:
			slip.TypePanic(s, depth, "address", args, "string", "(octets fixnum)")
		}
	}
	if sa == nil {
		slip.ErrorPanic(s, depth, "no address provided")
	}
	if err := syscall.Sendmsg(fd, buf, nil, sa, flags); err != nil {
		panic(err)
	}
	return len(buf)
}
