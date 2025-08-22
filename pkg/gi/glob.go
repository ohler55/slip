// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"path/filepath"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Glob{Function: slip.Function{Name: "glob", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "glob",
			Args: []*slip.DocArg{
				{
					Name: "function",
					Type: "symbol|lambda",
					Text: "The function to call for each matching filepath.",
				},
				{
					Name: "pattern",
					Type: "string",
					Text: "The filepath for the glob matches.",
				},
			},
			Return: "nil",
			Text: `__glob__ calls _function_ for each matching filepath with the argument
of the filepath.`,
			Examples: []string{
				"(let (paths)",
				` (glob (lambda (x) (addf path x)) "quux/*.lisp")`,
				` paths) => ("quux/a.lisp" "quux/b.lisp")`,
			},
		}, &Pkg)
}

// Glob represents the glob function.
type Glob struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Glob) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	d2 := depth + 1
	caller := cl.ResolveToCaller(s, args[0], d2)
	pat, ok := args[1].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "pattern", args[1], "string")
	}
	matches, err := filepath.Glob(string(pat))
	if err != nil {
		panic(err)
	}
	for _, m := range matches {
		_ = caller.Call(s, slip.List{slip.String(m)}, d2)
	}
	return nil
}
