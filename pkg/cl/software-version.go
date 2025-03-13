// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SoftwareVersion{Function: slip.Function{Name: "software-version", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "software-version",
			Args:   []*slip.DocArg{},
			Return: "string",
			Text:   `__software-version__ returns a string describing the lisp implementation version.`,
			Examples: []string{
				`(software-version) => "v1.0.0"`,
			},
		}, &slip.CLPkg)
}

// SoftwareVersion represents the software-version function.
type SoftwareVersion struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SoftwareVersion) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 0)

	return slip.String(version)
}
