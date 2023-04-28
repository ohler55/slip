// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := If{Function: slip.Function{Name: "if", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "if",
			Args: []*slip.DocArg{
				{
					Name: "test-form",
					Type: "object",
					Text: "The form to evaluate as the test.",
				},
				{
					Name: "then-form",
					Type: "object",
					Text: "The form to evaluate if _test-form_ evaluates to non-_nil_.",
				},
				{Name: "&optional"},
				{
					Name: "else-form",
					Type: "object",
					Text: "The form to evaluate if _test-form_ evaluates to _nil_.",
				},
			},
			Return: "object",
			Text: `__if__ evaluates the _then-forms_ if _test-form_ evaluates to non-_nil_ and returns
the result of the last form evaluated. If _else-form_ is present and _test-form_ evaluates to _nil_
then _else-form_ is evaluated and the result returned.`,
			Examples: []string{
				"(if t (+ 1 2) (+ 2 3)) => 3",
				"(if nil (+ 1 2) (+ 2 3)) => 5",
			},
		}, &slip.CLPkg)
}

// If represents the if function.
type If struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *If) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 2 {
		slip.PanicArgCount(f, 2, 3)
	}
	result = nil
	d2 := depth + 1
	pos := 0
	test := slip.EvalArg(s, args, pos, d2) != nil
	pos++
	if test {
		result = slip.EvalArg(s, args, pos, d2)
	} else if pos < len(args)-1 {
		pos++
		result = slip.EvalArg(s, args, pos, d2)
	}
	return
}
