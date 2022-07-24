// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := tStreamp{Function: slip.Function{Name: "streamp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "streamp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "nil",
			Text:   `__streamp__ returns _true_ if _object_ is a stream.`,
			Examples: []string{
				`(setq out (make-string-t-stream)`,
				"(t-stream-p out) => t",
				"(t-stream-p t) => nil",
			},
		}, &slip.CLPkg)
}

// tStreamp represents the t-stream-p function.
type tStreamp struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *tStreamp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if _, ok := args[0].(slip.Stream); ok {
		return slip.True
	}
	return nil
}
