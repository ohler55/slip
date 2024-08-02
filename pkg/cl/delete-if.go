// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DeleteIf{Function: slip.Function{Name: "delete-if", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "delete-if",
			Args: []*slip.DocArg{
				{
					Name: "test",
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
					Name: "count",
					Type: "fixnum",
					Text: `The maximum number of element to remove. Default is _nil_ for unlimited.`,
				},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _sequence_ to return a key for comparison.`,
				},
			},
			Return: "object",
			Text:   `__delete-if__ returns a sequence with the matching elements removed.`,
			Examples: []string{
				"(delete-if 'numberp '((y . 1) (1 . 2) (z . 3)) :key 'car) => ((y . 1) (z. 3))",
			},
		}, &slip.CLPkg)
}

// DeleteIf represents the delete-if function.
type DeleteIf struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *DeleteIf) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	var sfv seqFunVars
	sfv.setKeysIf(f, s, args, depth)

	switch ta := args[1].(type) {
	case nil:
		// nothing to delete-if
	case slip.List:
		result = f.inList(s, ta, depth, &sfv)
	case slip.String:
		result = f.inString(s, ta, depth, &sfv)
	case *slip.Vector:
		elements := f.inList(s, ta.AsList(), depth, &sfv)
		result = slip.NewVector(len(elements), ta.ElementType(), nil, elements, ta.Adjustable())
	case slip.Octets:
		result = f.inOctets(s, ta, depth, &sfv)
	default:
		slip.PanicType("sequence", ta, "sequence")
	}
	return
}

func (f *DeleteIf) inList(s *slip.Scope, seq slip.List, depth int, sfv *seqFunVars) (list slip.List) {
	if sfv.end < 0 || len(seq) < sfv.end {
		sfv.end = len(seq)
	}
	d2 := depth + 1
	var count int
	if sfv.fromEnd {
		for i := len(seq) - 1; 0 <= i; i-- {
			if i < sfv.start || sfv.end <= i || sfv.count <= count {
				list = append(list, seq[i])
				continue
			}
			key := seq[i]
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test.Call(s, slip.List{key}, d2) != nil {
				count++
				continue
			}
			list = append(list, seq[i])
		}
		// reverse list
		for i := len(list)/2 - 1; 0 <= i; i-- {
			list[i], list[len(list)-i-1] = list[len(list)-i-1], list[i]
		}
	} else {
		for i := 0; i < len(seq); i++ {
			if i < sfv.start || sfv.end <= i || sfv.count <= count {
				list = append(list, seq[i])
				continue
			}
			key := seq[i]
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test.Call(s, slip.List{key}, d2) != nil {
				count++
				continue
			}
			list = append(list, seq[i])
		}
	}
	return list
}

func (f *DeleteIf) inString(s *slip.Scope, seq slip.String, depth int, sfv *seqFunVars) slip.Object {
	ra := []rune(seq)
	if sfv.end < 0 || len(seq) < sfv.end {
		sfv.end = len(seq)
	}
	d2 := depth + 1
	var (
		count int
		nra   []rune
		key   slip.Object
	)
	if sfv.fromEnd {
		for i := len(ra) - 1; 0 <= i; i-- {
			if i < sfv.start || sfv.end <= i || sfv.count <= count {
				nra = append(nra, ra[i])
				continue
			}
			key = slip.Character(ra[i])
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test.Call(s, slip.List{key}, d2) != nil {
				count++
				continue
			}
			nra = append(nra, ra[i])
		}
		// reverse nra
		for i := len(nra)/2 - 1; 0 <= i; i-- {
			nra[i], nra[len(nra)-i-1] = nra[len(nra)-i-1], nra[i]
		}
	} else {
		for i := 0; i < len(ra); i++ {
			if i < sfv.start || sfv.end <= i || sfv.count <= count {
				nra = append(nra, ra[i])
				continue
			}
			key = slip.Character(ra[i])
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test.Call(s, slip.List{key}, d2) != nil {
				count++
				continue
			}
			nra = append(nra, ra[i])
		}
	}
	return slip.String(nra)
}

func (f *DeleteIf) inOctets(s *slip.Scope, seq slip.Octets, depth int, sfv *seqFunVars) slip.Object {
	ba := []byte(seq)
	if sfv.end < 0 || len(seq) < sfv.end {
		sfv.end = len(seq)
	}
	d2 := depth + 1
	var (
		count int
		nba   []byte
		key   slip.Object
	)
	if sfv.fromEnd {
		for i := len(ba) - 1; 0 <= i; i-- {
			if i < sfv.start || sfv.end <= i || sfv.count <= count {
				nba = append(nba, ba[i])
				continue
			}
			key = slip.Octet(ba[i])
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test.Call(s, slip.List{key}, d2) != nil {
				count++
				continue
			}
			nba = append(nba, ba[i])
		}
		// reverse nba
		for i := len(nba)/2 - 1; 0 <= i; i-- {
			nba[i], nba[len(nba)-i-1] = nba[len(nba)-i-1], nba[i]
		}
	} else {
		for i := 0; i < len(ba); i++ {
			if i < sfv.start || sfv.end <= i || sfv.count <= count {
				nba = append(nba, ba[i])
				continue
			}
			key = slip.Octet(ba[i])
			if sfv.key != nil {
				key = sfv.key.Call(s, slip.List{key}, d2)
			}
			if sfv.test.Call(s, slip.List{key}, d2) != nil {
				count++
				continue
			}
			nba = append(nba, ba[i])
		}
	}
	return slip.Octets(nba)
}
