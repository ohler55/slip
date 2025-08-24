// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := LambdaListKeywords{Function: slip.Function{Name: "lambda-list-keywords", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind:   slip.MacroSymbol,
			Name:   "lambda-list-keywords",
			Args:   []*slip.DocArg{},
			Return: "list",
			Text:   `__lambda-list-keywords__ returns a list of all the lambda list keywords in the implementation.`,
			Examples: []string{
				"(lambda-list-keywords) => (&allow-other-keys &aux &body &key &optional &rest)",
			},
		}, &slip.CLPkg)
}

// LambdaListKeywords represents the lambda-list-keywords function.
type LambdaListKeywords struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *LambdaListKeywords) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 0)

	return slip.List{
		slip.Symbol(slip.AmpAllowOtherKeys),
		slip.Symbol(slip.AmpAux),
		slip.Symbol(slip.AmpBody),
		slip.Symbol(slip.AmpKey),
		slip.Symbol(slip.AmpOptional),
		slip.Symbol(slip.AmpRest),
	}
}
