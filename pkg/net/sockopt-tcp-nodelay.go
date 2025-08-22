// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"syscall"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SockoptTcpNodelay{Function: slip.Function{Name: "sockopt-tcp-nodelay", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "sockopt-tcp-nodelay",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to get the option of.",
				},
			},
			Return: "fixnum|boolean",
			Text:   `__sockopt-tcp-nodelay__ returns the :keep-alive on the _socket_ instance.`,
			Examples: []string{
				`(sockopt-tcp-nodelay (make-instance 'socket :socket 5)) => nil`,
			},
		}, &Pkg)
}

// SockoptTcpNodelay represents the sockopt-tcp-nodelay function.
type SockoptTcpNodelay struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SockoptTcpNodelay) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA("socket") {
		slip.TypePanic(s, depth, "socket", args[0], "socket")
	}
	if fd, ok2 := self.Any.(int); ok2 {
		if val, _ := syscall.GetsockoptInt(fd, syscall.SOL_SOCKET, syscall.TCP_NODELAY); val != 0 {
			result = slip.True
		}
	}
	return
}

// Place a value in an option using the :set-option method.
func (f *SockoptTcpNodelay) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA("socket") {
		slip.TypePanic(s, 0, "socket", args[0], "socket")
	}
	if fd, ok2 := self.Any.(int); ok2 {
		if err := setSockoptBool(fd, syscall.TCP_NODELAY, value); err != nil {
			panic(err)
		}
	}
}
