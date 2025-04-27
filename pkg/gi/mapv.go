// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"math"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Mapv{Function: slip.Function{Name: "mapv", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "mapv",
			Args: []*slip.DocArg{
				{
					Name: "function",
					Type: "symbol|lambda",
					Text: "The function to call for each entry in _vectors_.",
				},
				{Name: "&rest"},
				{
					Name: "vectors",
					Type: "vector",
					Text: "The vectors to iterate over.",
				},
			},
			Return: "nil",
			Text: `__mapv__ calls _function_ for each entry in the _vectors_ with
an argument from each vector.`,
			Examples: []string{
				"(setq lst '())",
				"(mapv (lambda (x) (setq lst (cons x lst))) #(1 2 3)) => (1 2 3)",
				"lst => (3 2 1)",
			},
		}, &Pkg)
}

// Mapv represents the mapv function.
type Mapv struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Mapv) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, -1)
	fn := args[0]
	d2 := depth + 1
	caller := cl.ResolveToCaller(s, fn, d2)

	if 1 < len(args)-1 {
		min := math.MaxInt
		lists := make([]slip.List, len(args)-1)
		for i, arg := range args[1:] {
			list := f.asList(arg)
			lists[i] = list
			if len(list) < min {
				min = len(list)
			}
		}
		ca := make(slip.List, len(lists))
		for n := 0; n < min; n++ {
			for i, list := range lists {
				ca[i] = list[n]
			}
			_ = caller.Call(s, ca, d2)
		}
	} else {
		// The most common case.
		for _, v := range f.asList(args[1]) {
			_ = caller.Call(s, slip.List{v}, d2)
		}
	}
	return args[1]
}

func (f *Mapv) asList(v slip.Object) (list slip.List) {
	switch tv := v.(type) {
	case nil:
		// leave empty
	case slip.List:
		list = tv
	case slip.VectorLike:
		list = tv.AsList()
	default:
		slip.PanicType("vector", tv, "vector", "list")
	}
	return
}
