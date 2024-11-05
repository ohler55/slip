// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Case{Function: slip.Function{Name: "case", Args: args, SkipEval: []bool{false, true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "case",
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
or a list of values. A special case for the last clause allow for _otherwise_ or _t_.`,
				},
			},
			Return: "object",
			Text: `__case__ evaluates the keyform and then for Each clause _(keys form*)_ compares the
result of the _keyform_ to the keys of each form to determine of they are the same. If a match then the
forms of the clause are evaluated as if a progn and the final value returned. If the last clause keys is
_otherwise_ or _t_ then the forms in that clause are evaluated otherwise nil is return if there were no
other matches. The __equal__ function is used to determine a match which may be less strict than some
implementations.`,
			Examples: []string{
				"(setq value 3)",
				"(case value ((1 2) 'low) (3 'mid) (t 'high)) => mid",
			},
		}, &slip.CLPkg)
}

// Case represents the case function.
type Case struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Case) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	d2 := depth + 1
	key := args[0]
	for i, a := range args[1:] {
		clause, ok := a.(slip.List)
		if !ok || len(clause) == 0 {
			slip.PanicType("clause", a, "list")
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
			if !same && (k == slip.True || equal(k, slip.Symbol("otherwise"))) {
				if i != len(args)-2 {
					slip.NewPanic("bad case clause, not final %s", clause)
				}
				same = true
			}
		}
		if same {
			for i := 1; i < len(clause); i++ {
				result = slip.EvalArg(s, clause, i, d2)
			}
			break
		}
	}
	return
}
