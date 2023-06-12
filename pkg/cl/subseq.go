// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Subseq{Function: slip.Function{Name: "subseq", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "subseq",
			Args: []*slip.DocArg{
				{
					Name: "sequence",
					Type: "sequence",
					Text: `A sequence to take the sub-sequence of.`,
				},
				{
					Name: "tree-start",
					Type: "fixnum",
					Text: `The start of the sub-sequence.`,
				},
				{Name: "&optional"},
				{
					Name: "end",
					Type: "fixnum",
					Text: `The end of the sub-sequence. (default is the end of the string or _nil_)`,
				},
			},
			Return: "sequence",
			Text:   `__subseq__ returns the sub-sequence of the _sequence_ from _start_ to _end_.`,
			Examples: []string{
				`(subseq '(a b c d) 1 3) => (b c)`,
			},
		}, &slip.CLPkg)
}

// Subseq represents the subseq function.
type Subseq struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Subseq) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 3)
	start, ok := args[1].(slip.Fixnum)
	if !ok {
		slip.PanicType("start", args[1], "fixnum")
	}
	end := -1
	if 2 < len(args) {
		switch ta := args[2].(type) {
		case nil:
			// leave as -1
		case slip.Fixnum:
			end = int(ta)
			if end < 0 {
				slip.PanicType("end", args[2], "non negative fixnum")
			}
		default:
			slip.PanicType("end", args[2], "non negative fixnum")
		}
	}
	switch ta := args[0].(type) {
	case slip.List:
		if end < 0 {
			end = len(ta)
		}
		if start < 0 || len(ta) < int(start) || len(ta) < end {
			panic(fmt.Sprintf("indices %s and %d are out of bounds for list of length %d", start, end, len(ta)))
		}
		result = ta[start:end]
	case slip.String:
		ra := []rune(ta)
		if end < 0 {
			end = len(ra)
		}
		if start < 0 || len(ra) < int(start) || len(ra) < end {
			panic(fmt.Sprintf("indices %s and %d are out of bounds for string of length %d", start, end, len(ra)))
		}
		result = slip.String(ra[start:end])
	case slip.Vector:
		if end < 0 {
			end = len(ta)
		}
		if start < 0 || len(ta) < int(start) || len(ta) < end {
			panic(fmt.Sprintf("indices %s and %d are out of bounds for vector of length %d", start, end, len(ta)))
		}
		result = ta[start:end]
	default:
		slip.PanicType("sequence", ta, "sequence")
	}
	return
}

// Place a value in the first position of a list or cons.
func (f *Subseq) Place(args slip.List, value slip.Object) {
	slip.ArgCountCheck(f, args, 2, 3)

	// TBD

}
