// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := OctetsToString{Function: slip.Function{Name: "octets-to-string", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "octets-to-string",
			Args: []*slip.DocArg{
				{
					Name: "octets",
					Type: "list",
					Text: "list of octets to be converted into a string",
				},
			},
			Return: "string",
			Text:   `__octets-to-string__ converts a list of octets to a string`,
			Examples: []string{
				`(octets-to-string '(84 69 83 84 32 115 116 114 105 110 103) => "TEST string")`,
			},
		}, &Pkg)
}

// StringToOctets represents the string-to-octets function.
type OctetsToString struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *OctetsToString) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	list, ok := args[0].(slip.List)
	if !ok {
		slip.PanicType("octets", args[0], "list")
	}
	name := make([]byte, 0, len(list))
	for _, octet := range list {
		if n, ok := octet.(slip.Fixnum); ok {
			name = append(name, byte(int32(n)))
		} else {
			slip.PanicType("octet", octet, "integer")
		}
	}
	return slip.String(name)
}
