// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeSocket{Function: slip.Function{Name: "make-socket", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-socket",
			Args: []*slip.DocArg{
				{Name: "&key"},
				{
					Name: "domain",
					Type: "symbol",
					Text: socketArgText("domain", domainMap),
				},
				{
					Name: "type",
					Type: "symbol",
					Text: socketArgText("type", typeMap),
				},
				{
					Name: "protocol",
					Type: "symbol",
					Text: socketArgText("protocol", protocolMap),
				},
				{
					Name: "socket",
					Type: "fixnum",
					Text: "a socket file descriptot.",
				},
			},
			Return: "socket",
			Text: `__make-socket__ returns to a new _socket_ instance. An alternative to
_make-instance 'socket_.`,
			Examples: []string{
				`(make-socket :domain :unix :type :stream) => #<socket 1234>`,
			},
		}, &Pkg)
}

// MakeSocket represents the make-socket function.
type MakeSocket struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeSocket) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 6)
	self := socketFlavor.MakeInstance().(*flavors.Instance)

	_ = self.Receive(s, ":init", slip.List{args}, 0)

	return self
}
