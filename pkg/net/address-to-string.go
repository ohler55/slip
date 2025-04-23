// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"net/netip"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := AddressToString{Function: slip.Function{Name: "address-to-string", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "address-to-string",
			Args: []*slip.DocArg{
				{
					Name: "address",
					Type: "octets|list|vector",
					Text: "to return the string representation of.",
				},
			},
			Return: "string",
			Text:   `__address-to-string__ returns the string representation of _address_.`,
			Examples: []string{
				`(address-to-string #(127 0 0 1)) => "127.0.0.1"`,
				`(address-to-string #(32 3 13 183 0 0 0 0 0 0 0 0 0 2 0 1)) => "2003:db7::2:1"`,
			},
		}, &Pkg)
}

// AddressToString represents the address-to-string function.
type AddressToString struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *AddressToString) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	_, str := addressToString(args[0])
	return slip.String(str)
}

func addressToString(arg slip.Object) (oct slip.Octets, str string) {
	switch ta := arg.(type) {
	case slip.Octets:
		oct = ta
	case slip.List:
		oct = make(slip.Octets, len(ta))
		for i, v := range ta {
			oct[i] = byte(slip.ToOctet(v).(slip.Octet))
		}
	case slip.VectorLike:
		list := ta.AsList()
		oct = make(slip.Octets, len(list))
		for i, v := range list {
			oct[i] = byte(slip.ToOctet(v).(slip.Octet))
		}
	default:
		slip.PanicType("address", arg, "octets")
	}
	var addr netip.Addr
	switch len(oct) {
	case 4:
		addr = netip.AddrFrom4([4]byte(oct))
	case 16:
		addr = netip.AddrFrom16([16]byte(oct))
	default:
		slip.NewPanic("%s is neither an IPv4 nor an IPv6 address", oct)
	}
	str = addr.String()
	return
}
