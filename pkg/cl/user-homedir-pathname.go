// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"os"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := UserHomedirPathname{Function: slip.Function{Name: "user-homedir-pathname", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "user-homedir-pathname",
			Args:   []*slip.DocArg{},
			Return: "string",
			Text:   `__user-homedir-pathname__ returns the path to the user's home directory.`,
			Examples: []string{
				`(user-homedir-pathname) => "/Users/someone"`,
			},
		}, &slip.CLPkg)
}

// UserHomedirPathname represents the user-homedir-pathname function.
type UserHomedirPathname struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *UserHomedirPathname) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 0)

	dir, _ := os.UserHomeDir()

	return slip.String(dir)
}
