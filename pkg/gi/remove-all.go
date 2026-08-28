// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"os"

	"github.com/ohler55/slip"
)

func defRemoveAll() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := RemoveAll{Function: slip.Function{Name: "remove-all", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "remove-all",
			Args: []*slip.DocArg{
				{
					Name: "filepath",
					Type: "string",
					Text: "The file path to delete.",
				},
			},
			Return: "nil",
			Text:   `__remove-all__ deletes _filepath_ and any any children.`,
			Examples: []string{
				`(remove-all "something")`,
			},
		}, &Pkg)
}

// RemoveAll represents the remove-all function.
type RemoveAll struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *RemoveAll) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "string", args[0], "string")
	}
	if err := os.RemoveAll(string(path)); err != nil {
		slip.ErrorPanic(s, depth, "remove-all %s: %s", path, err)
	}
	return
}
