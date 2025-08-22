// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Range{Function: slip.Function{Name: "range", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "range",
			Args: []*slip.DocArg{
				{
					Name: "function",
					Type: "symbol|lambda",
					Text: "The function to call for each entry in _collection_.",
				},
				{
					Name: "collection",
					Type: "channel|list",
					Text: "The collection to iterate over.",
				},
			},
			Return: "nil",
			Text: `__range__ calls _function_ for each value in the _collection_ with an argument
from the _collection_. If a channel the iteration terminates when the channel is closed.`,
			Examples: []string{
				"(let ((c (make-channel 10)) result)",
				" (channel-push 1)",
				" (channel-push 2)",
				" (channel-close c)",
				" (range (lambda (x) (setq result (cons x result))) c)",
				" result) => (1 2)",
			},
		}, &Pkg)
}

// Range represents the range function.
type Range struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Range) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	d2 := depth + 1
	caller := cl.ResolveToCaller(s, args[0], d2)

	switch t1 := args[1].(type) {
	case slip.List:
		for _, v := range t1 {
			_ = caller.Call(s, slip.List{v}, d2)
		}
	case slip.Octets:
		for _, v := range t1 {
			_ = caller.Call(s, slip.List{slip.Octet(v)}, d2)
		}
	case slip.VectorLike:
		for _, v := range t1.AsList() {
			_ = caller.Call(s, slip.List{v}, d2)
		}
	case Ranger:
		t1.Range(s, caller, d2)
	default:
		slip.TypePanic(s, depth, "collection", t1, "channel", "list")
	}
	return nil
}
