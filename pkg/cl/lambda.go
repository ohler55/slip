// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Lambda{Function: slip.Function{Name: "lambda", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "lambda",
			Args: []*slip.DocArg{
				{
					Name: "lambda-list",
					Type: "list",
					Text: `A list of the function's argument. Arguments can be a symbol, a list of a symbol and a
default value, or one of &optional, &rest, &key, or &body. Arguments declarations following &optional are optional.
Argument declarations following &key represent key values. An argument declaration following &rest or &body indicates
one or more values follow.`,
				},
				{Name: slip.AmpOptional},
				{
					Name: "documentation",
					Type: "string",
					Text: "Documentation for the function.",
				},
				{Name: slip.AmpBody},
				{
					Name: "forms",
					Type: "object",
					Text: "The objects/forms to evaluate when the function is called.",
				},
			},
			Return: "object",
			Text:   `__lambda__ defines a function in the current package.`,
			Examples: []string{
				"(lambda () nil) => (lambda () nil)",
			},
		}, &slip.CLPkg)
}

// Lambda represents the lambda function.
type Lambda struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Lambda) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 {
		slip.PanicArgCount(f, 1, -1)
	}
	lc := slip.DefLambda("lambda", s, args)
	if s.Parent != nil {
		lc.Closure = s
	}
	return lc
}
