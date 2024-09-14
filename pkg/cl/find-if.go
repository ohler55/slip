// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FindIf{Function: slip.Function{Name: "find-if", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "find-if",
			Args: []*slip.DocArg{
				{
					Name: "predicate",
					Type: "symbol|lambda",
					Text: "The function to apply to each element of _sequence_.",
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
			Text:   `__find-if__ returns the first element that satisfies _test_ or _nil_ if there is no match.`,
			Examples: []string{
				"(find-if 'oddp '((x . 1) (y . 2) (z . 3)) :key 'cdr) => (x . 1)",
			},
		}, &slip.CLPkg)
}

// FindIf represents the find-if function.
type FindIf struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FindIf) Call(s *slip.Scope, args slip.List, depth int) (found slip.Object) {
	var sfv seqFunVars
	sfv.noCount = true
	sfv.setKeysIf(f, s, args, depth)

	switch ta := args[1].(type) {
	case nil:
		// nothing found
	case slip.List:
		found = f.inList(s, ta, depth, &sfv)
	case slip.String:
		found = f.inString(s, ta, depth, &sfv)
	case *slip.Vector:
		found = f.inList(s, ta.AsList(), depth, &sfv)
	case slip.Octets:
		found = f.inOctets(s, ta, depth, &sfv)
	default:
		slip.PanicType("sequence", ta, "sequence")
	}
	return
}

func (f *FindIf) inList(s *slip.Scope, seq slip.List, depth int, sfv *seqFunVars) slip.Object {
	if len(seq) <= sfv.start {
		return nil
	}
	if 0 <= sfv.end && sfv.end < len(seq) {
		seq = seq[sfv.start:sfv.end]
	} else {
		seq = seq[sfv.start:]
	}
	d2 := depth + 1
	if !sfv.fromEnd {
		for _, element := range seq {
			key := element
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test.Call(s, slip.List{key}, d2) != nil {
				return element
			}
		}
	}
	for i := len(seq) - 1; 0 <= i; i-- {
		key := seq[i]
		if sfv.key != nil {
			key = sfv.key.Call(s, slip.List{key}, d2)
		}
		if sfv.test.Call(s, slip.List{key}, d2) != nil {
			return seq[i]
		}
	}
	return nil
}

func (f *FindIf) inString(s *slip.Scope, seq slip.String, depth int, sfv *seqFunVars) (found slip.Object) {
	ra := []rune(seq)
	if len(ra) <= sfv.start {
		return nil
	}
	if 0 <= sfv.end && sfv.end < len(ra) {
		ra = ra[sfv.start:sfv.end]
	} else {
		ra = ra[sfv.start:]
	}
	d2 := depth + 1
	var key slip.Object
	if !sfv.fromEnd {
		for _, element := range ra {
			key = slip.Character(element)
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test.Call(s, slip.List{key}, d2) != nil {
				return slip.Character(element)
			}
		}
	}
	for i := len(ra) - 1; 0 <= i; i-- {
		key = slip.Character(ra[i])
		if sfv.key != nil {
			key = sfv.key.Call(s, slip.List{key}, d2)
		}
		if sfv.test.Call(s, slip.List{key}, d2) != nil {
			return slip.Character(ra[i])
		}
	}
	return nil
}

func (f *FindIf) inOctets(s *slip.Scope, seq slip.Octets, depth int, sfv *seqFunVars) (found slip.Object) {
	ba := []byte(seq)
	if len(ba) <= sfv.start {
		return nil
	}
	if 0 <= sfv.end && sfv.end < len(ba) {
		ba = ba[sfv.start:sfv.end]
	} else {
		ba = ba[sfv.start:]
	}
	d2 := depth + 1
	var key slip.Object
	if !sfv.fromEnd {
		for _, element := range ba {
			key = slip.Octet(element)
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test.Call(s, slip.List{key}, d2) != nil {
				return slip.Octet(element)
			}
		}
	}
	for i := len(ba) - 1; 0 <= i; i-- {
		key = slip.Octet(ba[i])
		if sfv.key != nil {
			key = sfv.key.Call(s, slip.List{key}, d2)
		}
		if sfv.test.Call(s, slip.List{key}, d2) != nil {
			return slip.Octet(ba[i])
		}
	}
	return nil
}
