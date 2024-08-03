// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Position{Function: slip.Function{Name: "position", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "position",
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
			Return: "fixnum|nil",
			Text: `__position__ returns the index of the first element that satisfies _test_
or _nil_ if there is no match.`,
			Examples: []string{
				"(position 'x '((x . 1) (y . 2) (z . 3)) :key 'car) => 0",
			},
		}, &slip.CLPkg)
}

// Position represents the position function.
type Position struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Position) Call(s *slip.Scope, args slip.List, depth int) (index slip.Object) {
	var sfv seqFunVars
	sfv.noCount = true
	sfv.setKeysItem(f, s, args, depth)
	switch ta := args[1].(type) {
	case nil:
		// nothing found
	case slip.List:
		index = f.inList(s, ta, depth, &sfv)
	case slip.String:
		index = f.inString(s, ta, depth, &sfv)
	case *slip.Vector:
		index = f.inList(s, ta.AsList(), depth, &sfv)
	case slip.Octets:
		index = f.inOctets(s, ta, depth, &sfv)
	default:
		slip.PanicType("sequence", ta, "sequence")
	}
	return
}

func (f *Position) inList(s *slip.Scope, seq slip.List, depth int, sfv *seqFunVars) slip.Object {
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
		for i, element := range seq {
			key := element
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test == nil {
				if slip.ObjectEqual(sfv.item, key) {
					return slip.Fixnum(sfv.start + i)
				}
			} else {
				if sfv.test.Call(s, slip.List{sfv.item, key}, d2) != nil {
					return slip.Fixnum(sfv.start + i)
				}
			}
		}
	}
	for i := len(seq) - 1; 0 <= i; i-- {
		key := seq[i]
		if sfv.key != nil {
			key = sfv.key.Call(s, slip.List{key}, d2)
		}
		if sfv.test == nil {
			if slip.ObjectEqual(sfv.item, key) {
				return slip.Fixnum(sfv.start + i)
			}
		} else {
			if sfv.test.Call(s, slip.List{sfv.item, key}, d2) != nil {
				return slip.Fixnum(sfv.start + i)
			}
		}
	}
	return nil
}

func (f *Position) inString(s *slip.Scope, seq slip.String, depth int, sfv *seqFunVars) (found slip.Object) {
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
		for i, element := range ra {
			key = slip.Character(element)
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test == nil {
				if slip.ObjectEqual(sfv.item, key) {
					return slip.Fixnum(sfv.start + i)
				}
			} else {
				if sfv.test.Call(s, slip.List{sfv.item, key}, d2) != nil {
					return slip.Fixnum(sfv.start + i)
				}
			}
		}
	}
	for i := len(ra) - 1; 0 <= i; i-- {
		key = slip.Character(ra[i])
		if sfv.key != nil {
			key = sfv.key.Call(s, slip.List{key}, d2)
		}
		if sfv.test == nil {
			if slip.ObjectEqual(sfv.item, key) {
				return slip.Fixnum(sfv.start + i)
			}
		} else {
			if sfv.test.Call(s, slip.List{sfv.item, key}, d2) != nil {
				return slip.Fixnum(sfv.start + i)
			}
		}
	}
	return nil
}

func (f *Position) inOctets(s *slip.Scope, seq slip.Octets, depth int, sfv *seqFunVars) (found slip.Object) {
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
		for i, element := range ba {
			key = slip.Octet(element)
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test == nil {
				if slip.ObjectEqual(sfv.item, key) {
					return slip.Fixnum(sfv.start + i)
				}
			} else {
				if sfv.test.Call(s, slip.List{sfv.item, key}, d2) != nil {
					return slip.Fixnum(sfv.start + i)
				}
			}
		}
	}
	for i := len(ba) - 1; 0 <= i; i-- {
		key = slip.Octet(ba[i])
		if sfv.key != nil {
			key = sfv.key.Call(s, slip.List{key}, d2)
		}
		if sfv.test == nil {
			if slip.ObjectEqual(sfv.item, key) {
				return slip.Fixnum(sfv.start + i)
			}
		} else {
			if sfv.test.Call(s, slip.List{sfv.item, key}, d2) != nil {
				return slip.Fixnum(sfv.start + i)
			}
		}
	}
	return nil
}
