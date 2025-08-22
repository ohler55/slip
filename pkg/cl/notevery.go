// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Notevery{Function: slip.Function{Name: "notevery", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "notevery",
			Args: []*slip.DocArg{
				{
					Name: "predicate",
					Type: "symbol|lambda",
					Text: `A function that takes as many argument as there are sequences.`,
				},
				{Name: "&rest"},
				{
					Name: "sequences",
					Type: "sequence",
					Text: "The sequences to iterate over.",
				},
			},
			Return: "boolean",
			Text: `__notevery__ returns _nil_ if the _predicate_ applied to each element of
_sequences_ in order returns true otherwise _true_.`,
			Examples: []string{
				`(notevery #'characterp "abc") => nil`,
				"(notevery '< '(1 2 3) '(2 1 4) '(4 5 6)) => t",
			},
		}, &slip.CLPkg)
}

// Notevery represents the notevery function.
type Notevery struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Notevery) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, -1)
	predicate := ResolveToCaller(s, args[0], depth)
	args = args[1:]
	cnt := len(args)
	d2 := depth + 1
	pargs := make(slip.List, cnt)
iter:
	for n := 0; true; n++ {
		for i := 0; i < cnt; i++ {
			switch ta := args[i].(type) {
			case slip.String:
				ra := []rune(ta)
				if len(ra) <= n {
					break iter
				}
				pargs[i] = slip.Character(ra[n])
			case slip.List:
				if len(ta) <= n {
					break iter
				}
				pargs[i] = ta[n]
			case slip.VectorLike:
				if ta.Length() <= n { // Length() for vectors is the same as Dimensions()[0]
					break iter
				}
				pargs[i] = ta.Get(n)
			default:
				slip.TypePanic(s, depth, "sequence", args[i], "string", "list", "vector")
			}
		}
		if predicate.Call(s, pargs, d2) == nil {
			return slip.True
		}
	}
	return nil
}
