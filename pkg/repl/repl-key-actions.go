// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"sort"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ReplKeyActions{Function: slip.Function{Name: "repl-key-actions", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "repl-key-actions",
			Args:   []*slip.DocArg{},
			Return: "list",
			Text: `__repl-key-actions__ returns a sorted list of the REPL editor actions that can
be bound to a key with _repl-bind-key_.`,
			Examples: []string{
				`(repl-key-actions) => (back-char back-word ...)`,
			},
		}, &Pkg)
}

// ReplKeyActions represents the repl-key-actions function.
type ReplKeyActions struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ReplKeyActions) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 0)
	names := make([]string, 0, len(actions))
	for name := range actions {
		names = append(names, name)
	}
	sort.Strings(names)
	list := make(slip.List, len(names))
	for i, name := range names {
		list[i] = slip.Symbol(name)
	}
	return list
}
