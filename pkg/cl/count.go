// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Count{Function: slip.Function{Name: "count", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "count",
			Args: []*slip.DocArg{
				{
					Name: "item",
					Type: "object",
					Text: "The value to match against the elements of _sequence_.",
				},
				{
					Name: "sequence",
					Type: "sequence",
					Text: "The sequence to search for _item_ in.",
				},
				{Name: "&key"},
				{
					Name: "from-end",
					Type: "boolean",
					Text: `If true the search is in reverse or or from the end to the start.`,
				},
				{
					Name: "test",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments and returns a boolean to
indicate a match. The default is _equal_`,
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
			Text:   `__count__ returns the number of elements in _sequence_ that match _item_.`,
			Examples: []string{
				"(count 'b '(a b c b a)) => 2",
			},
		}, &slip.CLPkg)
}

// Count represents the count function.
type Count struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Count) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	var sfv seqFunVars
	sfv.noCount = true
	sfv.setKeysItem(f, s, args, depth)

	switch ta := args[1].(type) {
	case nil:
		result = slip.Fixnum(0)
	case slip.List:
		result = slip.Fixnum(f.inList(s, ta, depth, &sfv))
	case slip.String:
		result = slip.Fixnum(f.inString(s, ta, depth, &sfv))
	case slip.VectorLike:
		result = slip.Fixnum(f.inList(s, ta.AsList(), depth, &sfv))
	default:
		slip.TypePanic(s, depth, "sequence", ta, "sequence")
	}
	return
}

func (f *Count) inList(s *slip.Scope, seq slip.List, depth int, sfv *seqFunVars) (count int) {
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
			if sfv.test == nil {
				if slip.ObjectEqual(sfv.item, key) {
					count++
				}
			} else {
				if sfv.test.Call(s, slip.List{sfv.item, key}, d2) != nil {
					count++
				}
			}
		}
	} else {
		for i := sfv.start; i < sfv.end; i++ {
			key := seq[i]
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test == nil {
				if slip.ObjectEqual(sfv.item, key) {
					count++
				}
			} else {
				if sfv.test.Call(s, slip.List{sfv.item, key}, d2) != nil {
					count++
				}
			}
		}
	}
	return
}

func (f *Count) inString(s *slip.Scope, seq slip.String, depth int, sfv *seqFunVars) (count int) {
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
			if sfv.test == nil {
				if slip.ObjectEqual(sfv.item, key) {
					count++
				}
			} else {
				if sfv.test.Call(s, slip.List{sfv.item, key}, d2) != nil {
					count++
				}
			}
		}
	} else {
		for i := sfv.start; i < sfv.end; i++ {
			key = slip.Character(ra[i])
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test == nil {
				if slip.ObjectEqual(sfv.item, key) {
					count++
				}
			} else {
				if sfv.test.Call(s, slip.List{sfv.item, key}, d2) != nil {
					count++
				}
			}
		}
	}
	return
}
