// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := InteractiveStreamP{Function: slip.Function{Name: "interactive-stream-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "interactive-stream-p",
			Args: []*slip.DocArg{
				{
					Name: "stream",
					Type: "stream",
					Text: "The stream to check.",
				},
			},
			Return: "nil",
			Text:   `__interactive-stream-p__ returns _true_ if _stream_ is an interactive stream.`,
			Examples: []string{
				`(setq out (make-string-output-stream)`,
				"(interactive-stream-p out) => nil",
				"(interactive-stream-p *standard-input*) => t",
			},
		}, &slip.CLPkg)
}

// InteractiveStreamP represents the interactive-stream-p function.
type InteractiveStreamP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *InteractiveStreamP) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if _, ok := args[0].(slip.Stream); !ok {
		slip.TypePanic(s, depth, "stream", args[0], "stream")
	}
	if args[0] == s.Get(slip.Symbol("*standard-input*")) {
		result = slip.True
	}
	return
}
