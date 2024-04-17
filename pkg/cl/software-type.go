// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SoftwareType{Function: slip.Function{Name: "software-type", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "software-type",
			Args:   []*slip.DocArg{},
			Return: "string",
			Text:   `__software-type__ returns a string describing the LISP software type.`,
			Examples: []string{
				`(software-type) => "SLIP - SLIce Processing for golang"`,
			},
		}, &slip.CLPkg)
}

// SoftwareType represents the software-type function.
type SoftwareType struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SoftwareType) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 0)
	return slip.String("SLIP - SLIce Processing for golang")
}
