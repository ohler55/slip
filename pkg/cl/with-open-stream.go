// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := WithOpenStream{
				Function: slip.Function{Name: "with-open-stream", Args: args, SkipEval: []bool{true}},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "with-open-stream",
			Args: []*slip.DocArg{
				{
					Name: "args",
					Type: "list",
					Text: `A list of _var_ and _stream_.`,
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "form",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object",
			Text:   `__with-open-stream__ evaluates the _forms_ with the _stream_ bound to _var_.`,
			Examples: []string{
				`(with-open-stream (s (make-string-input-stream "abc def")) (read s)) => abc`,
			},
		}, &slip.CLPkg)
}

// WithOpenStream represents the with-open-stream function.
type WithOpenStream struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *WithOpenStream) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	subArgs, ok := args[0].(slip.List)
	if !ok || len(subArgs) < 2 {
		slip.TypePanic(s, depth, "args", args[0], "list of (var stream)")
	}
	var sym slip.Symbol
	if sym, ok = subArgs[0].(slip.Symbol); !ok {
		slip.TypePanic(s, depth, "var", subArgs[0], "symbol")
	}
	var stream slip.Stream
	if stream, ok = slip.EvalArg(s, subArgs, 1, depth).(slip.Stream); ok {
		d2 := depth + 1
		s2 := s.NewScope()
		s2.Let(sym, stream)
		args = args[1:]
		for i := range args {
			result = slip.EvalArg(s2, args, i, d2)
		}
	} else {
		slip.TypePanic(s, depth, "stream", subArgs[1], "stream")
	}
	return
}
