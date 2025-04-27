// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Nreverse{Function: slip.Function{Name: "nreverse", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "nreverse",
			Args: []*slip.DocArg{
				{
					Name: "sequence",
					Type: "sequence",
					Text: `The _sequence_ to reverse.`,
				},
			},
			Return: "boolean",
			Text:   `__nreverse__ returns reverse of the sequence by modifying the _sequence_.`,
			Examples: []string{
				`(nreverse "abc") => "cba"`,
				"(nreverse '(a b c)) => (c b a)",
			},
		}, &slip.CLPkg)
}

// Nreverse represents the nreverse function.
type Nreverse struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Nreverse) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	switch ta := args[0].(type) {
	case nil:
		// leave as nil
	case slip.String:
		if 1 < len(ta) {
			ra := []rune(ta)
			max := len(ra) - 1
			for i := max / 2; 0 <= i; i-- {
				ra[i], ra[max-i] = ra[max-i], ra[i]
			}
			result = slip.String(ra)
		} else {
			result = ta
		}
	case slip.List:
		if 1 < len(ta) {
			max := len(ta) - 1
			for i := max / 2; 0 <= i; i-- {
				ta[i], ta[max-i] = ta[max-i], ta[i]
			}
		}
		result = ta
	case *slip.Vector:
		elements := ta.AsList()
		if 1 < len(elements) {
			max := len(elements) - 1
			for i := max / 2; 0 <= i; i-- {
				elements[i], elements[max-i] = elements[max-i], elements[i]
			}
		}
		result = ta
	case slip.Octets:
		if 1 < len(ta) {
			max := len(ta) - 1
			for i := max / 2; 0 <= i; i-- {
				ta[i], ta[max-i] = ta[max-i], ta[i]
			}
		}
		result = ta
	case *slip.BitVector:
		ta.Reverse()
		result = ta
	default:
		slip.PanicType("sequence", ta, "sequence")
	}
	return
}
