// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DeleteDuplicates{Function: slip.Function{Name: "delete-duplicates", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "delete-duplicates",
			Args: []*slip.DocArg{
				{
					Name: "sequence",
					Type: "sequence",
					Text: "The sequence to remove duplicates from.",
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
			Text:   `__delete-duplicates__ returns a sequence with duplicate elements removed.`,
			Examples: []string{
				"(delete-duplicates '(a b c b d a) => (c b d a)",
			},
		}, &slip.CLPkg)
}

// DeleteDuplicates represents the delete-duplicates function.
type DeleteDuplicates struct {
	slip.Function
}

type dupInfo struct {
	seqFunVars
	uniq []slip.Object
}

// Call the function with the arguments provided.
func (f *DeleteDuplicates) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	var di dupInfo
	di.noCount = true
	di.noItem = true
	di.setKeysItem(f, s, args, depth)

	switch ta := args[0].(type) {
	case nil:
		// nothing to delete-duplicates
	case slip.List:
		result = di.inList(s, ta, depth)
	case slip.String:
		result = di.inString(s, ta, depth)
	case *slip.Vector:
		elements := di.inList(s, ta.AsList(), depth)
		result = slip.NewVector(len(elements), ta.ElementType(), nil, elements, ta.Adjustable())
	default:
		slip.PanicType("sequence", ta, "sequence")
	}
	return
}

func (di *dupInfo) inList(s *slip.Scope, seq slip.List, depth int) (list slip.List) {
	if di.end < 0 || len(seq) < di.end {
		di.end = len(seq)
	}
	d2 := depth + 1
	// The last duplicate is kept for reverse the direction from what fromEnd
	// dictates.
	if di.fromEnd {
		for i := 0; i < len(seq); i++ {
			if i < di.start || di.end <= i {
				list = append(list, seq[i])
				continue
			}
			v := seq[i]
			if !di.has(s, v, d2) {
				list = append(list, v)
			}
		}
	} else {
		for i := len(seq) - 1; 0 <= i; i-- {
			if i < di.start || di.end <= i {
				list = append(list, seq[i])
				continue
			}
			v := seq[i]
			if !di.has(s, v, d2) {
				list = append(list, v)
			}
		}
		// reverse list
		for i := len(list)/2 - 1; 0 <= i; i-- {
			list[i], list[len(list)-i-1] = list[len(list)-i-1], list[i]
		}
	}
	return list
}

func (di *dupInfo) inString(s *slip.Scope, seq slip.String, depth int) slip.Object {
	ra := []rune(seq)
	if di.end < 0 || len(seq) < di.end {
		di.end = len(seq)
	}
	d2 := depth + 1
	var nra []rune
	if di.fromEnd {
		for i := 0; i < len(ra); i++ {
			if i < di.start || di.end <= i {
				nra = append(nra, ra[i])
				continue
			}
			if !di.has(s, slip.Character(ra[i]), d2) {
				nra = append(nra, ra[i])
			}
		}
	} else {
		for i := len(ra) - 1; 0 <= i; i-- {
			if i < di.start || di.end <= i {
				nra = append(nra, ra[i])
				continue
			}
			if !di.has(s, slip.Character(ra[i]), d2) {
				nra = append(nra, ra[i])
			}
		}
		// reverse nra
		for i := len(nra)/2 - 1; 0 <= i; i-- {
			nra[i], nra[len(nra)-i-1] = nra[len(nra)-i-1], nra[i]
		}
	}
	return slip.String(nra)
}

func (di *dupInfo) has(s *slip.Scope, v slip.Object, d2 int) bool {
	if di.key != nil {
		v = di.key.Call(s, slip.List{v}, d2)
	}
	if di.test == nil {
		for _, u := range di.uniq {
			if slip.ObjectEqual(v, u) {
				return true
			}
		}
	} else {
		for _, u := range di.uniq {
			if di.test.Call(s, slip.List{v, u}, d2) != nil {
				return true
			}
		}
	}
	di.uniq = append(di.uniq, v)
	return false
}
