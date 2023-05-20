// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"path/filepath"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FilepathBase{Function: slip.Function{Name: "filepath-base", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "filepath-base",
			Args: []*slip.DocArg{
				{
					Name: "filepath",
					Type: "string",
					Text: "The filepath to get the base from.",
				},
			},
			Return: "string",
			Text:   `__filepath-base__ returns the base of the _filepath_.`,
			Examples: []string{
				`(filepath-base "../one/two/three.lisp") => "three.lisp"`,
			},
		}, &slip.CLPkg)
}

// FilepathBase represents the filepath-base function.
type FilepathBase struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FilepathBase) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("string", args[0], "string")
	}
	return slip.String(filepath.Base(string(path)))
}
