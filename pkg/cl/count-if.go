// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CountIf{Function: slip.Function{Name: "count-if", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "count-if",
			Args: []*slip.DocArg{
				{
					Name: "predicate",
					Type: "object",
					Text: "The test to apply to each elements of _sequence_.",
				},
				{
					Name: "sequence",
					Type: "sequence",
					Text: "The sequence to search.",
				},
				{Name: "&key"},
				{
					Name: "from-end",
					Type: "boolean",
					Text: `If true the search is in reverse or or from the end to the start.`,
				},
				{
					Name: "start",
					Type: "fixnum",
					Text: `The index to the first element in the sequence to check. Defaults to 0`,
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: `The index to the last element in the sequence to check. Defaults to nil,
the length of the _sequence_.`,
				},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _sequence_ to return a key for comparison.`,
				},
			},
			Return: "object",
			Text:   `__count-if__ returns the number of elements in _sequence_ where _predicate_ returns non-nil.`,
			Examples: []string{
				"(count-if 'numberp '(a 1 b 2 c 3)) => 3",
			},
		}, &slip.CLPkg)
}

// CountIf represents the count-if function.
type CountIf struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CountIf) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	var sfv seqFunVars
	sfv.noCount = true
	sfv.setKeysIf(f, s, args, depth)

	switch ta := args[1].(type) {
	case nil:
		result = slip.Fixnum(0)
	case slip.List:
		result = slip.Fixnum(f.inList(s, ta, depth, &sfv))
	case slip.String:
		result = slip.Fixnum(f.inString(s, ta, depth, &sfv))
	case slip.Vector:
		result = slip.Fixnum(f.inList(s, slip.List(ta), depth, &sfv))
	default:
		slip.PanicType("sequence", ta, "sequence")
	}
	return
}

func (f *CountIf) inList(s *slip.Scope, seq slip.List, depth int, sfv *seqFunVars) (count int) {
	if sfv.end < 0 || len(seq) < sfv.end {
		sfv.end = len(seq)
	}
	d2 := depth + 1
	if sfv.fromEnd {
		for i := sfv.end - 1; sfv.start <= i; i-- {
			key := seq[i]
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test.Call(s, slip.List{key}, d2) != nil {
				count++
			}
		}
	} else {
		for i := sfv.start; i < sfv.end; i++ {
			key := seq[i]
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test.Call(s, slip.List{key}, d2) != nil {
				count++
			}
		}
	}
	return
}

func (f *CountIf) inString(s *slip.Scope, seq slip.String, depth int, sfv *seqFunVars) (count int) {
	ra := []rune(seq)
	if sfv.end < 0 || len(seq) < sfv.end {
		sfv.end = len(seq)
	}
	d2 := depth + 1
	var key slip.Object
	if sfv.fromEnd {
		for i := sfv.end - 1; sfv.start <= i; i-- {
			key = slip.Character(ra[i])
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test.Call(s, slip.List{key}, d2) != nil {
				count++
			}
		}
	} else {
		for i := sfv.start; i < sfv.end; i++ {
			key = slip.Character(ra[i])
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test.Call(s, slip.List{key}, d2) != nil {
				count++
			}
		}
	}
	return
}
