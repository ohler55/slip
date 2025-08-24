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
			f := SockoptBroadcast{Function: slip.Function{Name: "sockopt-broadcast", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "sockopt-broadcast",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to get the option of.",
				},
			},
			Return: "fixnum|boolean",
			Text:   `__sockopt-broadcast__ returns the :keep-alive on the _socket_ instance.`,
			Examples: []string{
				`(sockopt-broadcast (make-instance 'socket :socket 5)) => nil`,
			},
		}, &Pkg)
}

// SockoptBroadcast represents the sockopt-broadcast function.
type SockoptBroadcast struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SockoptBroadcast) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA("socket") {
		slip.TypePanic(s, depth, "socket", args[0], "socket")
	}
	if fd, ok2 := self.Any.(int); ok2 {
		if val, _ := syscall.GetsockoptInt(fd, syscall.SOL_SOCKET, syscall.SO_BROADCAST); val != 0 {
			result = slip.True
		}
	}
	return
}

// Place a value in an option using the :set-option method.
func (f *SockoptBroadcast) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.CheckArgCount(s, 0, f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA("socket") {
		slip.TypePanic(s, 0, "socket", args[0], "socket")
	}
	if fd, ok2 := self.Any.(int); ok2 {
		if err := setSockoptBool(fd, syscall.SO_BROADCAST, value); err != nil {
			panic(err)
		}
	}
}
