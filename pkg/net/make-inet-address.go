// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"net/netip"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeInetAddress{Function: slip.Function{Name: "make-inet-address", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-inet-address",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "address string to parse.",
				},
			},
			Return: "octets",
			Text:   `__make-inet-address__ returns the parsed IPv4 address. Panics on an invalid address.`,
			Examples: []string{
				`(make-inet-address "127.0.0.1") => #(127 0 0 1)`,
			},
		}, &Pkg)
}

// MakeInetAddress represents the make-inet-address function.
type MakeInetAddress struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeInetAddress) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	str, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "string", args[0], "string")
	}
	addr := slip.Octets(netip.MustParseAddr(string(str)).AsSlice())
	if len(addr) != 4 {
		slip.ErrorPanic(s, depth, "%s is not a valid IPv4 address", str)
	}
	return addr
}
