// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"net"
	"net/netip"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := GetHostByName{Function: slip.Function{Name: "get-host-by-name", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "get-host-by-name",
			Args: []*slip.DocArg{
				{
					Name: "node",
					Type: "string",
					Text: "name or ip address of host.",
				},
			},
			Return: "host-ent",
			Text:   `__get-host-by-name__ looks up a host by name or IP address.`,
			Examples: []string{
				`(get-host-by-name "localhost") => #<host-ent 12345>`,
			},
		}, &Pkg)
}

// GetHostByName represents the get-host-by-name function.
type GetHostByName struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *GetHostByName) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	node, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "node", args[0], "string")
	}
	if addrs, err := net.LookupHost(string(node)); err == nil {
		addresses := make(slip.List, len(addrs))
		for i, addr := range addrs {
			addresses[i] = slip.Octets(netip.MustParseAddr(addr).AsSlice())
		}
		result = makeHostEnt(node, addresses)
	}
	return
}
