// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := LispImplementationType{Function: slip.Function{Name: "lisp-implementation-type", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "lisp-implementation-type",
			Args:   []*slip.DocArg{},
			Return: "string",
			Text:   `__lisp-implementation-type__ returns a string describing the lisp implementation type.`,
			Examples: []string{
				`(lisp-implementation-type) => "SLIP - SLIce Processing for golang"`,
			},
		}, &slip.CLPkg)
}

// LispImplementationType represents the lisp-implementation-type function.
type LispImplementationType struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *LispImplementationType) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 0)
	return slip.String("SLIP - SLIce Processing for golang")
}
