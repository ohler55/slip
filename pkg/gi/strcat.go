// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Strcat{Function: slip.Function{Name: "strcat", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "strcat",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "strings",
					Type: "string",
					Text: "The strings to concatenate.",
				},
			},
			Return: "string",
			Text:   `__strcat__ returns all _strings_ concatenated together.`,
			Examples: []string{
				`(strcat 'string "abc" #\d '(#\e #\f)) => "abcdef"`,
			},
		}, &Pkg)
}

// Strcat represents the strcat function.
type Strcat struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Strcat) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, -1)

	var ra []rune
	for _, arg := range args {
	each:
		switch ta := arg.(type) {
		case nil:
			// empty list so ignore
		case slip.List:
			for _, a := range ta {
				if r, ok := a.(slip.Character); ok {
					ra = append(ra, rune(r))
				} else {
					slip.PanicType("list element", a, "character")
				}
			}
		case slip.String:
			ra = append(ra, []rune(ta)...)
		case slip.Character:
			ra = append(ra, rune(ta))
		case *slip.Vector:
			arg = ta.AsList()
			goto each
		default:
			slip.PanicType("&rest", ta, "string", "character")
		}
	}
	return slip.String(ra)
}
