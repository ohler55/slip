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
			f := SockoptReuseAddress{Function: slip.Function{Name: "sockopt-reuse-address", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "sockopt-reuse-address",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to get the option of.",
				},
			},
			Return: "fixnum|boolean",
			Text:   `__sockopt-reuse-address__ returns the :reuse-address on the _socket_ instance.`,
			Examples: []string{
				`(sockopt-reuse-address (make-instance 'socket :socket 5)) => nil`,
			},
		}, &Pkg)
}

// SockoptReuseAddress represents the sockopt-reuse-address function.
type SockoptReuseAddress struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SockoptReuseAddress) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA("socket") {
		slip.PanicType("socket", args[0], "socket")
	}
	if fd, ok2 := self.Any.(int); ok2 {
		if val, _ := syscall.GetsockoptInt(fd, syscall.SOL_SOCKET, syscall.SO_REUSEADDR); val != 0 {
			result = slip.True
		}
	}
	return
}

// Place a value in an option using the :set-option method.
func (f *SockoptReuseAddress) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA("socket") {
		slip.PanicType("socket", args[0], "socket")
	}
	if fd, ok2 := self.Any.(int); ok2 {
		if err := setSockoptBool(fd, syscall.SO_REUSEADDR, value); err != nil {
			panic(err)
		}
	}
}
