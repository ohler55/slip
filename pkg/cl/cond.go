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
	for i := len(args) - 1; 0 <= i; i-- {
		clause, ok := args[i].(slip.List)
		if !ok || len(clause) == 0 {
			slip.PanicType("clause", args[i], "list")
		}
		pos := len(clause) - 1
		if f.EvalArg(s, clause, pos, d2) == nil {
			continue
		}
		for pos--; 0 <= pos; pos-- {
			result = f.EvalArg(s, clause, pos, d2)
		}
		break
	}
	return
}
