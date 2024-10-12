// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"net"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := GetHostByAddress{Function: slip.Function{Name: "get-host-by-address", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "get-host-by-address",
			Args: []*slip.DocArg{
				{
					Name: "address",
					Type: "vector|octets|list",
					Text: "address of host.",
				},
			},
			Return: "host-ent",
			Text:   `__get-host-by-address__ looks up a host by address.`,
			Examples: []string{
				`(get-host-by-address #(127 0 0 1)) => #<host-ent 12345>`,
			},
		}, &Pkg)
}

// GetHostByAddress represents the get-host-by-address function.
type GetHostByAddress struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *GetHostByAddress) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	oct, addr := addressToString(args[0])
	if names, err := net.LookupAddr(addr); err == nil && 0 < len(names) {
		result = makeHostEnt(slip.String(names[0]), slip.List{oct})
	}
	return
}
