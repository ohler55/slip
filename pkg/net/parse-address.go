// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"net/netip"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ParseAddress{Function: slip.Function{Name: "parse-address", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "parse-address",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "address string to parse.",
				},
			},
			Return: "octets",
			Text:   `__parse-address__ returns the parsed address.`,
			Examples: []string{
				`(parse-address "127.0.0.1") => #(127 0 0 1)`,
				`(parse-address "2003:db7::2:1") => #(32 3 13 183 0 0 0 0 0 0 0 0 0 2 0 1)`,
			},
		}, &Pkg)
}

// ParseAddress represents the parse-address function.
type ParseAddress struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ParseAddress) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	str, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "string", args[0], "string")
	}
	return slip.Octets(netip.MustParseAddr(string(str)).AsSlice())
}
