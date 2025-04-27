// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeOctets{Function: slip.Function{Name: "make-octets", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-octets",
			Args: []*slip.DocArg{
				{
					Name: "size",
					Type: "fixnum",
					Text: "The size of the octets vector to create.",
				},
				{Name: "&optional"},
				{
					Name: "initial-element",
					Type: "octet",
					Text: "An initial value for the octets in the new octets vector. The default is 0.",
				},
			},
			Return: "octets",
			Text:   `__make-octets__ returns a octets vector initialized with _initial-element_.`,
			Examples: []string{
				`(make-octets 3) => #(0 0 0)`,
			},
		}, &Pkg)
}

// MakeOctets represents the make-octets function.
type MakeOctets struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeOctets) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 2)
	size, ok := args[0].(slip.Fixnum)
	if !ok || size < 0 {
		slip.PanicType("size", args[0], "fixnum")
	}
	ba := make([]byte, size)
	if 1 < len(args) {
		b := byte(slip.ToOctet(args[1]).(slip.Octet))
		for i := len(ba) - 1; 0 <= i; i-- {
			ba[i] = b
		}
	}
	return slip.Octets(ba)
}
