// Copyright (c) 2024, Peter Ohler, All rights reserved.

package repl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Completions{Function: slip.Function{Name: "completions", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "completions",
			Args: []*slip.DocArg{
				{
					Name: "prefix",
					Type: "string",
					Text: "The prefix to find matches for.",
				},
			},
			Return: "list",
			Text:   `__completions__ returns list of symbol names that strart with the provided prefix.`,
			Examples: []string{
				`(completions "prin1") => (prin1 prin1-to-string)`,
			},
		}, &Pkg)
}

// Completions represents the completions function.
type Completions struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Completions) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	prefix, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "prefix", args[0], "string")
	}
	var matches slip.List
	if words, lo, hi := WordMatch(string(prefix)); words != nil && lo <= hi {
		matches = make(slip.List, 0, hi-lo+1)
		for ; lo <= hi; lo++ {
			matches = append(matches, slip.String(words[lo]))
		}
	}
	return matches
}
