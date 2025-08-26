// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Elt{Function: slip.Function{Name: "elt", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "elt",
			Args: []*slip.DocArg{
				{
					Name: "sequence",
					Type: "sequence",
					Text: "The sequence to access an element of.",
				},
				{
					Name: "index",
					Type: "fixnum",
					Text: "The index into the sequence.",
				},
			},
			Return: "object",
			Text:   `__elt__ returns element of _sequence_ at _index_.`,
			Examples: []string{
				"(elt '(a b c) 1) => b",
				"(let ((seq '(a b c)))",
				" (setf (elt seq 1) 'x) => x",
				" seq) => (a x c)",
			},
		}, &slip.CLPkg)
}

// Elt represents the elt function.
type Elt struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Elt) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	index := getFixnumArg(s, args[1], "index", depth)
	switch seq := args[0].(type) {
	case slip.List:
		f.checkIndex(s, index, len(seq), depth)
		result = seq[index]
	case slip.Octets:
		f.checkIndex(s, index, len(seq), depth)
		result = slip.Octet(seq[index])
	case slip.String:
		ra := []rune(seq)
		f.checkIndex(s, index, len(ra), depth)
		result = slip.Character(ra[index])
	case slip.VectorLike:
		f.checkIndex(s, index, seq.Length(), depth)
		result = seq.Get(index)
	default:
		slip.TypePanic(s, depth, "sequence", seq, "sequence")
	}
	return
}

// Place a value in the first position of a list or cons.
func (f *Elt) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.CheckArgCount(s, 0, f, args, 2, 2)
	index := getFixnumArg(s, args[1], "index", 0)
	switch seq := args[0].(type) {
	case slip.List:
		f.checkIndex(s, index, len(seq), 0)
		seq[index] = value
	case slip.Octets:
		f.checkIndex(s, index, len(seq), 0)
		seq[index] = byte(slip.ToOctet(value).(slip.Octet))
	case slip.String:
		slip.ErrorPanic(s, 0, "setf on a string character is not possible")
	case slip.VectorLike:
		f.checkIndex(s, index, seq.Length(), 0)
		seq.Set(value, index)
	default:
		slip.TypePanic(s, 0, "sequence", seq, "sequence")
	}
}

func (f *Elt) checkIndex(s *slip.Scope, index, size, depth int) {
	if size <= index {
		slip.ErrorPanic(s, depth, "index %d is greater than the size of %d", index, size)
	}
}
