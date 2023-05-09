// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Concatenate{Function: slip.Function{Name: "concatenate", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "concatenate",
			Args: []*slip.DocArg{
				{
					Name: "result-type",
					Type: "symbol",
					Text: "The sequence specifier.",
				},
				{Name: "&rest"},
				{
					Name: "sequences",
					Type: "string|list",
					Text: "The sequences to concatenate.",
				},
			},
			Return: "sequence",
			Text: `__concatenate__ returns a sequence of the designated _result-type_ that
is a concatenation of all the remaining arguments coerced into the _result-type_ elements.`,
			Examples: []string{
				`(concatenate 'string "abc" #\d '(#\e #\f)) => "abcdef"`,
				`(concatenate 'list "abc" '(d e f) #(1 2 3)) => (#\a #\b #\c d e f 1 2 3)`,
			},
		}, &slip.CLPkg)
}

// Concatenate represents the concatenate function.
type Concatenate struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Concatenate) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, -1)
	switch args[0] {
	case slip.Symbol("list"):
		result = f.listConc(args[1:])
	case slip.Symbol("string"):
		result = f.stringConc(args[1:])
	case slip.Symbol("vector"):
		result = slip.Vector(f.listConc(args[1:]))
	default:
		slip.PanicType("result-type", args[0], "symbol (list, string, or vector)")
	}
	return
}

func (f *Concatenate) listConc(args slip.List) (result slip.List) {
	for _, arg := range args {
		switch ta := arg.(type) {
		case nil:
			// empty list so ignore
		case slip.List:
			result = append(result, ta...)
		case slip.String:
			for _, r := range []rune(ta) {
				result = append(result, slip.Character(r))
			}
		case slip.Vector:
			result = append(result, ta...)
		default:
			slip.PanicType("&rest", ta, "list", "string", "vector")
		}
	}
	return
}

func (f *Concatenate) stringConc(args slip.List) slip.String {
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
		case slip.Vector:
			arg = slip.List(ta)
			goto each
		default:
			slip.PanicType("&rest", ta, "list", "string", "vector")
		}
	}
	return slip.String(ra)
}
