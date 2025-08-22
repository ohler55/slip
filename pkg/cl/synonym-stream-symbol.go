// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SynonymStreamSymbolf{Function: slip.Function{Name: "synonym-stream-symbol", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "synonym-stream-symbol",
			Args: []*slip.DocArg{
				{
					Name: "synonym-stream",
					Type: "synonym-stream",
					Text: "The synonym-stream to return the symbol of.",
				},
			},
			Return: "list",
			Text:   `__synonym-stream-symbol__ returns the symbol of _synonym-stream_`,
			Examples: []string{
				`(synonym-stream-symbol (make-synonym-stream 'zz)) => zz`,
			},
		}, &slip.CLPkg)
}

// SynonymStreamSymbolf represents the synonym-stream-symbol function.
type SynonymStreamSymbolf struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SynonymStreamSymbolf) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	ss, ok := args[0].(*SynonymStream)
	if !ok {
		slip.TypePanic(s, depth, "synonym-stream", args[0], "synonym-stream")
	}
	return ss.symbol
}
