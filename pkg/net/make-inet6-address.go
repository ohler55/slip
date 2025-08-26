// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"net/netip"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeInet6Address{Function: slip.Function{Name: "make-inet6-address", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-inet6-address",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "address string to parse.",
				},
			},
			Return: "octets",
			Text:   `__make-inet6-address__ returns the parsed IPv6 address. Panics on an invalid address.`,
			Examples: []string{
				`(parse-address "2003:db7::2:1") => #(32 3 13 183 0 0 0 0 0 0 0 0 0 2 0 1)`,
			},
		}, &Pkg)
}

// MakeInet6Address represents the make-inet6-address function.
type MakeInet6Address struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeInet6Address) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	str, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "string", args[0], "string")
	}
	addr := slip.Octets(netip.MustParseAddr(string(str)).AsSlice())
	if len(addr) != 16 {
		slip.ErrorPanic(s, depth, "%s is not a valid IPv6 address", str)
	}
	return addr
}
