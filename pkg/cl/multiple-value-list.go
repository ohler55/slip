// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MultipleValueList{
				Function: slip.Function{Name: "multiple-value-list", Args: args, SkipEval: []bool{true}},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "multiple-value-list",
			Args: []*slip.DocArg{
				{
					Name: "form",
					Type: "form",
					Text: "The form to evaluate.",
				},
			},
			Return: "object",
			Text:   `__multiple-value-list__ returns a list of the values returned by _form_.`,
			Examples: []string{
				"(multiple-value-list (floor 5 3)) => (1 2)",
			},
		}, &slip.CLPkg)
}

// MultipleValueList represents the multiple-value-list function.
type MultipleValueList struct {
	slip.Function
}

// List the function with the arguments provided.
func (f *MultipleValueList) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	d2 := depth + 1
	form := args[0]
	if list, ok := form.(slip.List); ok {
		form = slip.ListToFunc(s, list, d2)
	}
	v := s.Eval(form, d2)
	if vs, ok := v.(slip.Values); ok {
		result = slip.List(vs)
	} else {
		result = slip.List{v}
	}
	return
}
