// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"path/filepath"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Namestring{Function: slip.Function{Name: "namestring", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "namestring",
			Args: []*slip.DocArg{
				{
					Name: "filepath",
					Type: "string",
					Text: "The file to get the absolute for.",
				},
			},
			Return: "string|nil",
			Text:   `__namestring__ returns fuul absolute path for _filepath_.`,
			Examples: []string{
				`(namestring "three.lisp") => "/one/two/three.lisp"`,
			},
		}, &slip.CLPkg)
}

// Namestring represents the namestring function.
type Namestring struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Namestring) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "filepath", args[0], "string")
	}
	abs, _ := filepath.Abs(string(path))

	return slip.String(abs)
}
