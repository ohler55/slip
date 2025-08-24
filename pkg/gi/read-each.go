// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"io"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ReadEach{Function: slip.Function{Name: "read-each", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "read-each",
			Args: []*slip.DocArg{
				{
					Name: "input-stream",
					Type: "input-stream",
					Text: "The stream to read from.",
				},
				{
					Name: "function",
					Type: "symbol|lambda",
					Text: "The function to call for each expression read.",
				},
			},
			Return: "nil",
			Text:   `__read-each__ calls _function_ for each s-expression read from the _input-stream_.`,
			Examples: []string{
				`(let (quux)`,
				` (read-each (make-string-input-stream "(1 2 3)") (lambda (x) (setq quux x)))`,
				` quux) => (1 2 3)`,
			},
		}, &Pkg)
}

// ReadEach represents the read-each function.
type ReadEach struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ReadEach) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	r, ok := args[0].(io.Reader)
	if !ok {
		slip.TypePanic(s, depth, "input-stream", args[0], "input-stream")
	}
	caller := cl.ResolveToCaller(s, args[1], depth+1)
	slip.ReadStreamEach(r, s, caller)

	return nil
}
