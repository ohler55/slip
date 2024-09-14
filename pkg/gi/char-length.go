// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CharLength{Function: slip.Function{Name: "char-length", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "char-length",
			Args: []*slip.DocArg{
				{
					Name: "sequence",
					Type: "sequence",
					Text: "The sequence to count characters in.",
				},
				{Name: "&key"},
				{
					Name: "start",
					Type: "fixnum",
					Text: "the start of the _sequence_ to count chars in. default: 0",
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: "the end of the _sequence_ to count chars in. default: nil (end of the string)",
				},
			},
			Return: "fixnum",
			Text:   `__char-length__ returns the length of _sequence_ in chars.`,
			Examples: []string{
				`(char-length (coerce "Test" 'octets)) => 4`,
				`(char-length (coerce #(84 69 83 84 95 227 129 180) 'octets)) => 6 ;; TEST_ぴ`,
			},
		}, &Pkg)
}

// CharLength represents the char-length function.
type CharLength struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CharLength) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 5)
	start, end := seqStarEndArgs(args)
	var size int
	a0 := args[0]
top:
	switch ta := a0.(type) {
	case nil:
		// size remains at zero
	case slip.Octets:
		size = len([]rune(string(ta[start:end])))
	case slip.String:
		size = end - start
	case slip.List:
		size = end - start
		for _, v := range ta[start:end] {
			if _, ok := v.(slip.Character); !ok {
				slip.PanicType("list element", v, "character")
			}
		}
	case *slip.Vector:
		a0 = ta.AsList()
		goto top
	default:
		slip.PanicType("sequence", args[0], "octets", "string", "list of characters")
	}
	return slip.Fixnum(size)
}
