// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"unicode/utf8"

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
					Name: "string",
					Type: "string",
					Text: "The string that will be converted to octets.",
				},
				{Name: "&key"},
				{
					Name: "start",
					Type: "fixnum",
					Text: "the start of the _string_ to convert to octets. default: 0",
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: "the end of the _string_ to convert to octets. default: nil (end of the string)",
				},
			},
			Return: "octets",
			Text:   `__string-to-octets__ converts a string to octets. _string_ can also be a list of characters.`,
			Examples: []string{
				`(string-to-octets "TEST string") => #(84 69 83 84 32 115 116 114 105 110 103)`,
			},
		}, &Pkg)
}

// StringToOctets represents the string-to-octets function.
type StringToOctets struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *StringToOctets) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 5)
	start, end := seqStarEndArgs(args)
	a0 := args[0]
top:
	switch ta := a0.(type) {
	case nil:
		result = slip.Octets{}
	case slip.String:
		ra := []rune(ta)
		ba := make([]byte, 0, end-start)
		for _, r := range ra[start:end] {
			ba = utf8.AppendRune(ba, r)
		}
		result = slip.Octets(ba)
	case slip.List:
		ba := make([]byte, 0, end-start)
		for _, v := range ta[start:end] {
			if c, ok := v.(slip.Character); ok {
				ba = utf8.AppendRune(ba, rune(c))
			} else {
				slip.PanicType("string element", v, "character")
			}
		}
		result = slip.Octets(ba)
	case slip.VectorLike:
		a0 = ta.AsList()
		goto top
	default:
		slip.PanicType("string", args[0], "string", "list of characters")
	}
	return
}
