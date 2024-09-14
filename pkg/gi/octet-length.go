// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"unicode/utf8"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := OctetLength{Function: slip.Function{Name: "octet-length", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "octet-length",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "The string to count octets in.",
				},
				{Name: "&key"},
				{
					Name: "start",
					Type: "fixnum",
					Text: "the start of the _string_ to count octets in. default: 0",
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: "the end of the _string_ to count octets in. default: nil (end of the string)",
				},
			},
			Return: "fixnum",
			Text: `__octet-length__ returns the length of _string_ in octets.
_string_ can also be a list of characters.`,
			Examples: []string{
				`(octet-length "Test") => 4`,
				`(octet-length "Test_ぴ_") => 9`,
			},
		}, &Pkg)
}

// OctetLength represents the octet-length function.
type OctetLength struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *OctetLength) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 5)
	start, end := seqStarEndArgs(args)
	var size int
	a0 := args[0]
top:
	switch ta := a0.(type) {
	case nil:
		// size remains at zero
	case slip.String:
		ra := []rune(ta)
		for _, r := range ra[start:end] {
			size += utf8.RuneLen(r)
		}
	case slip.List:
		for _, v := range ta[start:end] {
			if c, ok := v.(slip.Character); ok {
				size += utf8.RuneLen(rune(c))
			} else {
				slip.PanicType("string element", v, "character")
			}
		}
	case *slip.Vector:
		a0 = ta.AsList()
		goto top
	default:
		slip.PanicType("string", args[0], "string", "list of characters")
	}
	return slip.Fixnum(size)
}
