// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := The{Function: slip.Function{Name: "the", Args: args, SkipEval: []bool{true, true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "the",
			Args: []*slip.DocArg{
				{
					Name: "value-type",
					Type: "symbol",
					Text: "The return type.",
				},
				{
					Name: "form",
					Type: "form",
					Text: "The form to call.",
				},
			},
			Return: "object",
			Text: `__the__ calls _form_ and then verifies the result is a _value-type_. If not
an error is raised if the result is not a _value-type_.`,
			Examples: []string{
				`(the fixnum (+ 1 2)) => 3`,
			},
		}, &slip.CLPkg)
}

// The represents the the function.
type The struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *The) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	result = slip.EvalArg(s, args, 1, 0)
	valueIsA(s, result, args[0], depth)
	return
}

// Place a value place indicated by the form.
func (f *The) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.CheckArgCount(s, 0, f, args, 2, 2)
	valueIsA(s, value, args[0], 0)
	placeExprValue(s, args[1], value, 0)
}

func valueIsA(s *slip.Scope, value, valueType slip.Object, depth int) {
	var match bool
	if valueType != slip.True && value != nil {
		if vt, ok := valueType.(slip.Symbol); ok {
			for _, h := range value.Hierarchy() {
				if strings.EqualFold(string(h), string(vt)) {
					match = true
					break
				}
			}
			if !match {
				slip.NewPanic("The value %s is not of type %s.", value, vt)
			}
		} else {
			slip.TypePanic(s, depth, "value-type", valueType, "symbol", "t")
		}
	}
}
