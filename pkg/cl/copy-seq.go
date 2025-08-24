// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CopySeq{Function: slip.Function{Name: "copy-seq", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "copy-seq",
			Args: []*slip.DocArg{
				{
					Name: "sequence",
					Type: "sequence",
					Text: "The sequence to copy.",
				},
			},
			Return: "list",
			Text: `__copy-seq__ returns a copy of _sequence_ with all elements the
same as _sequence_. This is a shallow copy.`,
			Examples: []string{
				"(copy-seq '(a b c)) => (a b c)",
				"(setq lst '(a b c))",
				"(eq lst (copy-seq lst)) => nil",
				"(equal lst (copy-seq lst)) => t",
			},
		}, &slip.CLPkg)
}

// CopySeq represents the copy-seq function.
type CopySeq struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CopySeq) Call(s *slip.Scope, args slip.List, depth int) (seq slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	switch ta := args[0].(type) {
	case nil:
		// leave as nil
	case slip.String:
		seq = ta
	case slip.List:
		list := make(slip.List, len(ta))
		copy(list, ta)
		seq = list
	case *slip.Vector:
		vv := slip.NewVector(ta.Length(), ta.ElementType(), nil, ta.Elements(), ta.Adjustable())
		vv.FillPtr = ta.FillPtr
		seq = vv
	case slip.Octets:
		dup := make(slip.Octets, len(ta))
		copy(dup, ta)
		seq = dup
	case *slip.BitVector:
		seq = ta.Duplicate()
	default:
		slip.TypePanic(s, depth, "sequence", args[0], "sequence")
	}
	return seq
}
