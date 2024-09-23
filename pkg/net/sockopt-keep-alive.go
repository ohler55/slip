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
			f := SockoptKeepAlive{Function: slip.Function{Name: "sockopt-keep-alive", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "sockopt-keep-alive",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to get the option of.",
				},
			},
			Return: "fixnum|boolean",
			Text:   `__sockopt-keep-alive__ returns the :keep-alive on the _socket_ instance.`,
			Examples: []string{
				`(sockopt-keep-alive (make-instance 'socket :socket 5)) => nil`,
			},
		}, &Pkg)
}

// SockoptKeepAlive represents the sockopt-keep-alive function.
type SockoptKeepAlive struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SockoptKeepAlive) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA(socketFlavor) {
		slip.PanicType("socket", args[0], "socket")
	}
	if fd, ok2 := self.Any.(int); ok2 {
		if val, _ := syscall.GetsockoptInt(fd, syscall.SOL_SOCKET, syscall.SO_KEEPALIVE); val != 0 {
			result = slip.True
		}
	}
	return
}

// Place a value in an option using the :set-option method.
func (f *SockoptKeepAlive) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA(socketFlavor) {
		slip.PanicType("socket", args[0], "socket")
	}
	if fd, ok2 := self.Any.(int); ok2 {
		if err := setSockoptBool(fd, syscall.SO_KEEPALIVE, value); err != nil {
			panic(err)
		}
	}
}
