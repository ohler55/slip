// Copyright (c) 2024, Peter Ohler, All rights reserved.

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
					Type: "octets|list",
					Text: "octets or a list of objects that can be coerced into an octet",
				},
				{Name: "&key"},
				{
					Name: "start",
					Type: "fixnum",
					Text: "the start of the _octets_ to convert to a string. default: 0",
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: "the end of the _octets_ to convert to a string. default: nil (end of the octets)",
				},
			},
			Return: "string",
			Text:   `__octets-to-string__ converts _octets_ to a string`,
			Examples: []string{
				`(octets-to-string '(84 69 83 #\T 32 115 116 114 105 110 103) => "TEST string")`,
				`(octets-to-string (coerce '(84 69 83 84) 'octets)) => "TEST")`,
			},
		}, &Pkg)
}

// OctetsToString represents the octets-to-string function.
type OctetsToString struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *OctetsToString) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 5)
	start, end := seqStarEndArgs(args)
	a0 := args[0]
top:
	switch ta := a0.(type) {
	case nil:
		result = slip.String("")
	case slip.Octets:
		result = slip.String(ta[start:end])
	case slip.List:
		octs := make(slip.Octets, end-start)
		for i, v := range ta[start:end] {
			octs[i] = byte(slip.ToOctet(v).(slip.Octet))
		}
		result = slip.String(octs)
	case slip.VectorLike:
		a0 = ta.AsList()
		goto top
	default:
		slip.PanicType("octets", args[0], "list", "vector", "octets")
	}
	return
}
