// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Notany{Function: slip.Function{Name: "notany", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "notany",
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
			Text: `__notany__ returns _true_ if the _predicate_ applied to each element of
_sequences_ in order returns nil.`,
			Examples: []string{
				`(notany #'characterp "abc") => nil`,
				"(notany '< '(1 2 3) '(2 3 4) '(4 5 6)) => nil",
				"(notany '> '(1 2 3) '(1 3 4)) => t",
			},
		}, &slip.CLPkg)
}

// Notany represents the notany function.
type Notany struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Notany) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, -1)
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
			case *slip.Vector:
				if ta.Size() <= n { // Size() for vectors is the same as Dimensions()[0]
					break iter
				}
				pargs[i] = ta.Get(n)
			default:
				slip.PanicType("sequence", args[i], "string", "list", "vector")
			}
		}
		if predicate.Call(s, pargs, d2) != nil {
			return nil
		}
	}
	return slip.True
}
