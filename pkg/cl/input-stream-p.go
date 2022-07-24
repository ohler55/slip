// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := InputStreamp{Function: slip.Function{Name: "input-stream-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "input-stream-p",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "nil",
			Text:   `__input-stream-p__ returns _true_ if _object_ is an input stream.`,
			Examples: []string{
				`(setq in (make-string-input-stream "abc")`,
				"(input-stream-p in) => t",
				"(input-stream-p t) => nil",
			},
		}, &slip.CLPkg)
}

// InputStreamp represents the input-stream-p function.
type InputStreamp struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *InputStreamp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if _, ok := args[0].(io.Reader); ok {
		return slip.True
	}
	return nil
}
