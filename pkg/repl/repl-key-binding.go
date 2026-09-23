// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ReplKeyBinding{Function: slip.Function{Name: "repl-key-binding", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "repl-key-binding",
			Args: []*slip.DocArg{
				{
					Name: "key",
					Type: "string",
					Text: `The key name such as "C-l", "M-f", or "M-[1;5C".`,
				},
			},
			Return: "symbol",
			Text: `__repl-key-binding__ returns the action bound to _key_ in the REPL editor. A
user binding is returned if there is one otherwise the default binding is
returned. If _key_ is not bound or has been disabled _nil_ is returned.`,
			Examples: []string{
				`(repl-key-binding "C-a") => line-begin`,
			},
		}, &Pkg)
}

// ReplKeyBinding represents the repl-key-binding function.
type ReplKeyBinding struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ReplKeyBinding) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	_, seqs := parseKeyArg(s, depth, args[0])

	return actionSymbol(keyAction(seqs[0]))
}
