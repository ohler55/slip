// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringToOctets{Function: slip.Function{Name: "string-to-octets", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "string-to-octets",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "string",
					Text: "The string that will be converted to a list of octets.",
				},
			},
			Return: "list of octets",
			Text:   `__string-to-octets__ converts a string to a list of octets.`,
			Examples: []string{
				`(string-to-octets "TEST string") => (#(84 69 83 84 32 115 116 114 105 110 103)`,
			},
		}, &Pkg)
}

// StringToOctets represents the string-to-octets function.
type StringToOctets struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *StringToOctets) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	var (
		name string
	)
	if vs, ok := args[0].(slip.String); ok {
		name = string(vs)
	} else {
		slip.PanicType("name", args[0], "string")
	}
	list := make(slip.List, len(name))
	for i, octet := range name {
		list[i] = slip.Fixnum(octet)
	}
	return list
}
