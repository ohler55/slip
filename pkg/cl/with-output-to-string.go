// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := WithOutputToString{
				Function: slip.Function{Name: "with-output-to-string", Args: args, SkipEval: []bool{true}},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "with-output-to-string",
			Args: []*slip.DocArg{
				{
					Name: "args",
					Type: "list",
					Text: `A list of _var_.`,
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "form",
					Text: "The forms to evaluate.",
				},
			},
			Return: "string",
			Text: `__with-output-to-string__ evaluates the _forms_ with output string stream bound
to _var_. Unlike Common LISP there is no string-form optional argument no is an element type supported
as the element type is always _character_.`,
			Examples: []string{
				`(with-output-to-string (s) (princ "abc" s)) => "abc"`,
			},
		}, &slip.CLPkg)
}

// WithOutputToString represents the with-output-to-string function.
type WithOutputToString struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *WithOutputToString) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	subArgs, ok := args[0].(slip.List)
	if !ok || len(subArgs) < 1 {
		slip.TypePanic(s, depth, "args", args[0], "list of (var &optional string-form)")
	}
	var sym slip.Symbol
	if sym, ok = subArgs[0].(slip.Symbol); !ok {
		slip.TypePanic(s, depth, "var", subArgs[0], "symbol")
	}
	var out strings.Builder
	stream := slip.OutputStream{Writer: &out}
	d2 := depth + 1
	s2 := s.NewScope()
	s2.Let(sym, &stream)
	args = args[1:]
	for i := range args {
		_ = slip.EvalArg(s2, args, i, d2)
	}
	return slip.String(out.String())
}
