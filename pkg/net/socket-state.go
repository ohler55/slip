// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSocketState() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SocketState{Function: slip.Function{Name: "socket-state", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-state",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to return the state of.",
				},
			},
			Return: "nil|:read-write|:read|:write",
			Text:   `__socket-state__ returns the state of a _socket_ instance.`,
			Examples: []string{
				`(let ((sock (make-instance 'socket)))`,
				`  (socket-state sock)) => nil`,
			},
		}, &Pkg)
}

// SocketState represents the socket-state function.
type SocketState struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketState) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if self, ok := args[0].(*flavors.Instance); ok {
		result = socketState(self)
	}
	return
}

type socketStateCaller struct{}

func (caller socketStateCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.ArgCountCheck(self, args, 0, 0)
	return socketState(self)
}

func (caller socketStateCaller) FuncDocs() *slip.FuncDoc {
	md := methodDocFromFunc(":state", "socket-state", &Pkg)
	md.Examples[len(md.Examples)-1] = `  (send sock :state)) => nil`
	return md
}

func socketState(self *flavors.Instance) (result slip.Object) {
	if fd, ok := self.Any.(int); ok {
		var (
			rset FdSet
			wset FdSet
		)
		rset.Set(fd)
		wset.Set(fd)
		if err := Select(&rset, &wset, nil, 0); err == nil {
			if rset.IsSet(fd) {
				if wset.IsSet(fd) {
					result = slip.Symbol(":read-write")
				} else {
					result = slip.Symbol(":read")
				}
			} else if wset.IsSet(fd) {
				result = slip.Symbol(":write")
			}
		}
	}
	return
}
