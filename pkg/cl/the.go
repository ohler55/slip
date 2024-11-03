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
	slip.ArgCountCheck(f, args, 2, 2)
	result = slip.EvalArg(s, args, 1, 0)
	valueIsA(result, args[0])
	return
}

// Place a value place indicated by the form.
func (f *The) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	valueIsA(value, args[0])

	if placer, ok := args[1].(slip.Placer); ok {
		pargs := placer.GetArgs()
		sargs := make(slip.List, len(pargs))
		for i := range pargs {
			sargs[i] = slip.EvalArg(s, pargs, i, 0)
		}
		placer.Place(s, sargs, value)
	}
}

func valueIsA(value, valueType slip.Object) {
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
			slip.PanicType("value-type", valueType, "symbol", "t")
		}
	}
}
