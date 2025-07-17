// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSocketOpenp() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SocketOpenp{Function: slip.Function{Name: "socket-open-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-open-p",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to check.",
				},
			},
			Return: "nil|",
			Text:   `__socket-open-p__ return true if the _socket_ instance is open.`,
			Examples: []string{
				`(let ((sock (make-instance 'socket)))`,
				`  (socket-open-p sock)) => nil`,
			},
		}, &Pkg)
}

// SocketOpenp represents the socket-open-p function.
type SocketOpenp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketOpenp) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA("socket") {
		slip.PanicType("socket", args[0], "socket")
	}
	if _, ok = self.Any.(int); ok {
		result = slip.True
	}
	return
}

type socketOpenpCaller struct{}

func (caller socketOpenpCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.SendArgCountCheck(self, ":open-p", args, 0, 0)
	if _, ok := self.Any.(int); ok {
		result = slip.True
	}
	return
}

func (caller socketOpenpCaller) FuncDocs() *slip.FuncDoc {
	md := methodDocFromFunc(":open-p", "socket-open-p", &Pkg)
	md.Examples[len(md.Examples)-1] = `  (send sock :open-p)) => nil`
	return md
}
