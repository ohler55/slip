// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Nsubstitute{Function: slip.Function{Name: "nsubstitute", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "nsubstitute",
			Args: []*slip.DocArg{
				{
					Name: "new",
					Type: "object",
					Text: "The replacement value.",
				},
				{
					Name: "old",
					Type: "object",
					Text: "The value to replace.",
				},
				{
					Name: "sequence",
					Type: "sequemce",
					Text: "The sequence to replace values in.",
				},
				{Name: "&key"},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _alist_ to return a key for comparison. The same function is also applied to _item_.`,
				},
				{
					Name: "test",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments; the _item_ and each element
in the list at _place_. A return of false will cause _item_ to be prepended.`,
				},
				{
					Name: "from-end",
					Type: "boolean",
					Text: `If true start from the end. Only applies when _count_ is non-nil.`,
				},
				{
					Name: "start",
					Type: "fixnum",
					Text: `The index of the start of replacements.`,
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: `The index of the end (exclusive) of replacements.`,
				},
				{
					Name: "count",
					Type: "fixnum",
					Text: `The maximum number of replacements.`,
				},
			},
			Return: "sequence",
			Text:   `__nsubstitute__ returns a copy _sequence_ with _old_ nsubstituted with _new_.`,
			Examples: []string{
				"(setq lst '(a b c)",
				"(nsubstitute 2 'b lst) => (a 2 c)",
				"lst => (a b c)",
			},
		}, &slip.CLPkg)
}

// Nsubstitute represents the nsubstitute function.
type Nsubstitute struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Nsubstitute) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	sr := parseSubstituteArgs(f, s, args, depth)
	switch seq := args[2].(type) {
	case nil:
		// nothing to replace
	case slip.List:
		result = sr.replace(seq)
	case slip.String:
		if _, ok := sr.rep.(slip.Character); !ok {
			slip.TypePanic(s, depth, "new", sr.rep, "character")
		}
		ra := []rune(seq)
		dup := make(slip.List, len(ra))
		for i, r := range ra {
			dup[i] = slip.Character(r)
		}
		_ = sr.replace(dup)
		for i, v := range dup {
			ra[i] = rune(v.(slip.Character))
		}
		result = slip.String(ra)
	case *slip.Vector:
		elements := seq.AsList()
		_ = sr.replace(elements)
		result = seq
	case slip.Octets:
		if _, ok := sr.rep.(slip.Octet); !ok {
			slip.TypePanic(s, depth, "new", sr.rep, "Octet")
		}
		result = sr.replaceBytes(seq)
	default:
		slip.TypePanic(s, depth, "sequence", seq, "sequence")
	}
	return
}
