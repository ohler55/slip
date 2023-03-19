// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cond{Function: slip.Function{Name: "cond", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "cond",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "clause*",
					Type: "list",
					Text: `Each clause _(test-form form*)_ takes the form of a test-form and
zero of more forms to be evaluate if the _test-form_ evalues to no-_nil_.`,
				},
			},
			Return: "object",
			Text: `__cond__ evaluates each _clause_ until a _clause_ _test-form_ evaluates to
non-_nil_. The rest of the _form*_ in the _clause_ are evaluated and the result of the last
evaluation is returned.`,
			Examples: []string{
				"(setq value 3)",
				"(cond ((evenp value) 'even) (t 'odd)) => odd",
			},
		}, &slip.CLPkg)
}

// Cond represents the cond function.
type Cond struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Cond) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	result = nil
	d2 := depth + 1
	for _, a := range args {
		clause, ok := a.(slip.List)
		if !ok || len(clause) == 0 {
			slip.PanicType("clause", a, "list")
		}
		if f.EvalArg(s, clause, 0, d2) == nil {
			continue
		}
		for i := 1; i < len(clause); i++ {
			result = f.EvalArg(s, clause, i, d2)
		}
		break
	}
	return
}
