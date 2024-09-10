// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"syscall"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/clos"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	// TBD form Text from  options
	//   keyword, doc, type? (or just use the arg
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
					Type: "usocket",
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
  :tcp-keepaline
  :tcp-nodelay
  :broadcast
  :reuse-address
  :send-timeout
  :send-buffer
  :receive-timeout
  :receive-buffer
`,
			Examples: []string{
				`(socket-option (make-instance 'usocket :socket 5) :tcp-keepalive) => t`,
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
	if !ok || self.Flavor != usocketFlavor {
		slip.PanicType("socket", args[0], "usocket")
	}
	if fd, ok2 := self.Any.(int); ok2 {
		result = getSockopt(fd, args[1])
	}
	return
}

// TBD Placer also

type usocketOptionCaller struct{}

func (caller usocketOptionCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":option", args, 1, 1)
	if fd, ok := self.Any.(int); ok {
		result = getSockopt(fd, args[0])
	}
	return
}

func (caller usocketOptionCaller) Docs() string {
	return clos.MethodDocFromFunc(":option", "socket-option", "usocket", "socket")
}

func getSockopt(fd int, arg slip.Object) (result slip.Object) {
	var (
		val int
		err error
	)
	switch arg {
	case slip.Symbol(":tcp-keepaline"): // SO_KEEPALIVE
		val, err = syscall.GetsockoptInt(fd, syscall.SOL_SOCKET, syscall.SO_KEEPALIVE)
		if val != 0 {
			result = slip.True
		}
	case slip.Symbol(":tcp-nodelay"): // TCP_NODELAY
		val, err = syscall.GetsockoptInt(fd, syscall.IPPROTO_TCP, syscall.TCP_NODELAY)
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
	default:
		slip.PanicType("option", arg,
			":tcp-keepaline",
			":tcp-nodelay",
			":broadcast",
			":reuse-address",
			":send-timeout",
			":send-buffer",
			":receive-timeout",
			":receive-buffer",
		)
	}
	if err != nil {
		panic(err)
	}
	return
}
