// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Length{Function: slip.Function{Name: "length", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "length",
			Args: []*slip.DocArg{
				{
					Name: "sequence",
					Type: "sequence",
					Text: `The _sequence_ to return the length of. Deviating from the LISP _length_ the length
of other types such as _hash-table_ and other collections are also supported without raising an exception.`,
				},
			},
			Return: "boolean",
			Text:   `__length__ returns length of the sequence.`,
			Examples: []string{
				`(length "abc") => 3`,
				"(length '(a b)) => 2",
			},
		}, &slip.CLPkg)
}

// Length represents the length function.
type Length struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Length) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	switch ta := args[0].(type) {
	case nil:
		result = slip.Fixnum(0)
	case HasLength:
		result = slip.Fixnum(ta.Length())
	default:
		slip.PanicType("sequence", ta, "sequence")
	}
	return
}
