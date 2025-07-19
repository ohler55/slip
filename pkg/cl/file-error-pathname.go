// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

const pathnameSymbol = slip.Symbol("pathname")

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FileErrorPathname{Function: slip.Function{Name: "file-error-pathname", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "file-error-pathname",
			Args: []*slip.DocArg{
				{
					Name: "condition",
					Type: "file-error",
					Text: "The file-error or subclass of file-error to get the pathname of.",
				},
			},
			Return: "object",
			Text: `__file-error-pathname__ returns the value of the _pathname_ slot in the _condition_
which must be of file _file-error_ or inherit from _file-error_.`,
			Examples: []string{
				`(file-error-pathname (make-condition 'file-error :pathname 'test)) => test)`,
			},
		}, &slip.CLPkg)
}

// FileErrorPathname represents the file-error-pathname function.
type FileErrorPathname struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FileErrorPathname) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if ci, ok := args[0].(slip.Instance); !ok || !ci.IsA("file-error") {
		slip.PanicType("file-error", args[0], "file-error")
	} else {
		result, _ = ci.SlotValue(pathnameSymbol)
	}
	return
}
