// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"fmt"
	"syscall"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/clos"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SocketOption{Function: slip.Function{Name: "socket-option", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-option",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to get the option of.",
				},
				{
					Name: "option",
					Type: "keyword",
					Text: "the option identifier.",
				},
			},
			Return: "fixnum|boolean",
			Text: `__socket-option__ returns the value of the option on the _socket_ instance. The
supported options are:
  :broadcast
  :debug
  :receive-buffer
  :receive-timeout
  :reuse-address
  :send-buffer
  :send-timeout
  :tcp-keepalive
  :tcp-nodelay
`,
			Examples: []string{
				`(socket-option (make-instance 'socket :socket 5) :tcp-keepalive) => t`,
			},
		}, &Pkg)
}

// SocketOption represents the socket-option function.
type SocketOption struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketOption) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA(socketFlavor) {
		slip.PanicType("socket", args[0], "socket")
	}
	if fd, ok2 := self.Any.(int); ok2 {
		result = getSockopt(fd, args[1])
	}
	return
}

// Place a value in an option using the :set-option method.
func (f *SocketOption) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA(socketFlavor) {
		slip.PanicType("socket", args[0], "socket")
	}
	_ = self.Receive(s, ":set-option", slip.List{args[1], value}, 0)
}

type socketOptionCaller struct{}

func (caller socketOptionCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":option", args, 1, 1)
	if fd, ok := self.Any.(int); ok {
		result = getSockopt(fd, args[0])
	}
	return
}

func (caller socketOptionCaller) Docs() string {
	return clos.MethodDocFromFunc(":option", "socket-option", "socket", "socket")
}

type socketSetOptionCaller struct{}

func (caller socketSetOptionCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":option", args, 2, 2)
	if fd, ok := self.Any.(int); ok {
		setSockopt(fd, args[0], args[1])
	}
	return nil
}

func (caller socketSetOptionCaller) Docs() string {
	return clos.MethodDocFromFunc(":set-option", "socket-option", "socket", "socket")
}

func getSockopt(fd int, arg slip.Object) (result slip.Object) {
	var (
		val int
		err error
	)
	switch arg {
	case slip.Symbol(":tcp-keepalive"): // SO_KEEPALIVE
		val, err = syscall.GetsockoptInt(fd, syscall.SOL_SOCKET, syscall.SO_KEEPALIVE)
		if val != 0 {
			result = slip.True
		}
	case slip.Symbol(":tcp-nodelay"): // TCP_NODELAY
		val, err = syscall.GetsockoptInt(fd, syscall.SOL_SOCKET, syscall.TCP_NODELAY)
		if val != 0 {
			result = slip.True
		}
	case slip.Symbol(":broadcast"): // SO_BROADCAST
		val, err = syscall.GetsockoptInt(fd, syscall.SOL_SOCKET, syscall.SO_BROADCAST)
		if val != 0 {
			result = slip.True
		}
	case slip.Symbol(":reuse-address"): // SO_REUSEADDR
		val, err = syscall.GetsockoptInt(fd, syscall.SOL_SOCKET, syscall.SO_REUSEADDR)
		if val != 0 {
			result = slip.True
		}
	case slip.Symbol(":send-timeout"): // SO_SNDTIMEO
		val, err = syscall.GetsockoptInt(fd, syscall.SOL_SOCKET, syscall.SO_SNDTIMEO)
		result = slip.Fixnum(val)
	case slip.Symbol(":send-buffer"): // SO_SNDBUF
		val, err = syscall.GetsockoptInt(fd, syscall.SOL_SOCKET, syscall.SO_SNDBUF)
		result = slip.Fixnum(val)
	case slip.Symbol(":receive-timeout"): // SO_RCVTIMEO
		val, err = syscall.GetsockoptInt(fd, syscall.SOL_SOCKET, syscall.SO_RCVTIMEO)
		result = slip.Fixnum(val)
	case slip.Symbol(":receive-buffer"): // SO_RCVBUF
		val, err = syscall.GetsockoptInt(fd, syscall.SOL_SOCKET, syscall.SO_RCVBUF)
		result = slip.Fixnum(val)
	case slip.Symbol(":debug"): // TCP_DEBUG
		val, err = syscall.GetsockoptInt(fd, syscall.SOL_SOCKET, syscall.SO_DEBUG)
		if val != 0 {
			result = slip.True
		}
	default:
		slip.PanicType("option", arg,
			":broadcast",
			":debug",
			":receive-buffer",
			":receive-timeout",
			":reuse-address",
			":send-buffer",
			":send-timeout",
			":tcp-keepalive",
			":tcp-nodelay",
		)
	}
	if err != nil {
		slip.NewPanic("option %s failed. %s", arg, err)
	}
	return
}

func setSockopt(fd int, opt, val slip.Object) {
	var err error
	switch opt {
	case slip.Symbol(":tcp-keepalive"): // SO_KEEPALIVE
		err = setSockoptBool(fd, syscall.SO_KEEPALIVE, val)
	case slip.Symbol(":tcp-nodelay"): // TCP_NODELAY
		err = setSockoptBool(fd, syscall.TCP_NODELAY, val)
	case slip.Symbol(":broadcast"): // SO_BROADCAST
		err = setSockoptBool(fd, syscall.SO_BROADCAST, val)
	case slip.Symbol(":reuse-address"): // SO_REUSEADDR
		err = setSockoptBool(fd, syscall.SO_REUSEADDR, val)
	case slip.Symbol(":send-timeout"): // SO_SNDTIMEO
		err = setSockoptTime(fd, syscall.SO_SNDTIMEO, val)
	case slip.Symbol(":send-buffer"): // SO_SNDBUF
		err = setSockoptInt(fd, syscall.SO_SNDBUF, val)
	case slip.Symbol(":receive-timeout"): // SO_RCVTIMEO
		err = setSockoptTime(fd, syscall.SO_RCVTIMEO, val)
	case slip.Symbol(":receive-buffer"): // SO_RCVBUF
		err = setSockoptInt(fd, syscall.SO_RCVBUF, val)
	case slip.Symbol(":debug"): // SO_DEBUG
		err = setSockoptBool(fd, syscall.SO_DEBUG, val)
	default:
		slip.PanicType("option", opt,
			":broadcast",
			":debug",
			":receive-buffer",
			":receive-timeout",
			":reuse-address",
			":send-buffer",
			":send-timeout",
			":tcp-keepalive",
			":tcp-nodelay",
		)
	}
	if err != nil {
		slip.NewPanic("option %s failed. %s", opt, err)
	}
	return
}

func setSockoptBool(fd int, opt int, val slip.Object) error {
	var vi int
	if val == slip.True || val == slip.Fixnum(1) {
		vi = 1
	}
	return syscall.SetsockoptInt(fd, syscall.SOL_SOCKET, opt, vi)
}

func setSockoptInt(fd int, opt int, val slip.Object) error {
	num, ok := val.(slip.Fixnum)
	if !ok {
		return fmt.Errorf("must be a fixnum not %s", val)
	}
	return syscall.SetsockoptInt(fd, syscall.SOL_SOCKET, opt, int(num))
}

func setSockoptTime(fd int, opt int, val slip.Object) error {
	r, ok := val.(slip.Real)
	if !ok {
		return fmt.Errorf("must be a real not %s", val)
	}
	tv := syscall.NsecToTimeval(int64(r.RealValue() * float64(time.Second)))

	return syscall.SetsockoptTimeval(fd, syscall.SOL_SOCKET, opt, &tv)
}
