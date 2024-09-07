// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"fmt"
	"net"

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
			Return: "nil|",
			Text: `__socket-option__ returns the value of the option on the _socket_ instance. The
supported options are:

`,
			Examples: []string{
				`(socket-option (make-instance 'usocket)) => #<xxx>`,
			},
		}, &Pkg)
}

// SocketOption represents the socket-option function.
type SocketOption struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketOption) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	self, ok := args[0].(*flavors.Instance)
	if !ok || self.Flavor != usocketFlavor {
		slip.PanicType("socket", args[0], "usocket")
	}
	if nc, ok2 := self.Any.(net.Conn); ok2 {
		// TBD switch on option, use fcntl or getsockopt, maybe sys/unix/fcntl.go or just syscall
		//  getting File is a copy so can't set options
		//  for listener set with listener control config before starting
		// alternately switch to using syscall
		fmt.Printf("*** %v\n", nc)
	}
	return nil
}

// TBD Placer also

type usocketOptionCaller struct{}

func (caller usocketOptionCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":option", args, 1, 1)
	if nc, ok2 := self.Any.(net.Conn); ok2 {
		// TBD
		fmt.Printf("*** %v\n", nc)
	}
	return nil
}

func (caller usocketOptionCaller) Docs() string {
	return clos.MethodDocFromFunc(":option", "socket-option", "usocket", "socket")
}
