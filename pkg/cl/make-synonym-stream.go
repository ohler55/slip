// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeSynonymStream{Function: slip.Function{Name: "make-synonym-stream", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-synonym-stream",
			Args: []*slip.DocArg{
				{
					Name: "symbol",
					Type: "symbol",
					Text: "Symbol that names a global variable.",
				},
			},
			Return: "synonym-stream",
			Text: `__make-synonym-stream__ returns an _synonym-stream_ with the _symbol_ as the synonym.
Note that unlike common lisp the _symbol_ must name a global and not a scoped variable such as in a __let__ block.`,
			Examples: []string{
				`(defvar zz (make-string-output-stream))`,
				`(make-synonym-stream 'zz) => #<SYNONYM-STREAM :symbol zz>`,
			},
		}, &slip.CLPkg)
}

// MakeSynonymStream represents the make-synonym-stream function.
type MakeSynonymStream struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeSynonymStream) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "symbol", args[0], "symbol")
	}
	return NewSynonymStream(sym)
}
