// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Ecase{Function: slip.Function{Name: "ecase", Args: args, SkipEval: []bool{false, true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "ecase",
			Args: []*slip.DocArg{
				{
					Name: "keyform*",
					Type: "object",
					Text: `form to evaluate for the key to the case function.`,
				},
				{Name: "&rest"},
				{
					Name: "clause*",
					Type: "list",
					Text: `Clauses are lists of the form _(keys form*)_ where keys can be a single value
or a list of values.`,
				},
			},
			Return: "object",
			Text: `__ecase__ evaluates the keyform and then for Each clause _(keys form*)_ compares the
result of the _keyform_ to the keys of each form to determine of they are the same. If a match then the
forms of the clause are evaluated as if a progn and the final value returned. If there is no match a
type error is raised.`,
			Examples: []string{
				"(setq value 3)",
				"(ecase value ((1 2) 'low) (3 'mid)) => mid",
			},
		}, &slip.CLPkg)
}

// Ecase represents the ecase function.
type Ecase struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Ecase) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	d2 := depth + 1
	key := args[0]
	var found bool // this approach is needed to achieve 100% coverage
	for _, a := range args[1:] {
		clause, ok := a.(slip.List)
		if !ok || len(clause) == 0 {
			slip.TypePanic(s, depth, "clause", a, "list")
		}
		var same bool
		if keys, ok := clause[0].(slip.List); ok {
			for _, k := range keys {
				same = equal(key, k)
				if same {
					break
				}
			}
		} else {
			k := clause[0]
			same = equal(key, k)
		}
		if same {
			for i := 1; i < len(clause); i++ {
				result = slip.EvalArg(s, clause, i, d2)
			}
			found = true
			break
		}
	}
	if !found {
		var wants []string
		for _, a := range args[1:] {
			clause := a.(slip.List)
			if keys, ok := clause[0].(slip.List); ok {
				for _, k := range keys {
					wants = append(wants, slip.ObjectString(k))
				}
			} else {
				wants = append(wants, slip.ObjectString(clause[0]))
			}
		}
		slip.TypePanic(s, depth, "key", key, wants...)
	}
	return
}
