// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Reverse{Function: slip.Function{Name: "reverse", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "reverse",
			Args: []*slip.DocArg{
				{
					Name: "sequence",
					Type: "sequence",
					Text: `The _sequence_ to reverse.`,
				},
			},
			Return: "boolean",
			Text:   `__reverse__ returns the reverse of the _sequence_ in a new _sequence_.`,
			Examples: []string{
				`(reverse "abc") => "cba"`,
				"(reverse '(a b c)) => (c b a)",
			},
		}, &slip.CLPkg)
}

// Reverse represents the reverse function.
type Reverse struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Reverse) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	result = args[0]
	switch ta := args[0].(type) {
	case nil:
		// leave as nil
	case slip.String:
		if 0 < len(ta) {
			ra := []rune(ta)
			nl := make([]rune, len(ra))
			copy(nl, ra)
			max := len(ra) - 1
			for i := max / 2; 0 <= i; i-- {
				nl[i], nl[max-i] = nl[max-i], nl[i]
			}
			result = slip.String(nl)
		}
	case slip.List:
		if 0 < len(ta) {
			nl := make(slip.List, len(ta))
			copy(nl, ta)
			max := len(ta) - 1
			for i := max / 2; 0 <= i; i-- {
				nl[i], nl[max-i] = nl[max-i], nl[i]
			}
			result = nl
		}
	case *slip.Vector:
		elements := ta.AsList()
		if 1 < len(elements) {
			nl := make(slip.List, len(elements))
			copy(nl, elements)
			max := len(elements) - 1
			for i := max / 2; 0 <= i; i-- {
				nl[i], nl[max-i] = nl[max-i], nl[i]
			}
			result = slip.NewVector(len(nl), ta.ElementType(), nil, nl, ta.Adjustable())
		}
	case slip.Octets:
		nl := make(slip.Octets, len(ta))
		last := len(ta) - 1
		for i, v := range ta {
			nl[last-i] = v
		}
		result = nl
	default:
		slip.PanicType("sequence", ta, "sequence")
	}
	return
}
