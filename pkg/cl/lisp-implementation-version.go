// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"runtime/debug"

	"github.com/ohler55/slip"
)

var version = ""

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := LispImplementationVersion{Function: slip.Function{Name: "lisp-implementation-version", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "lisp-implementation-version",
			Args:   []*slip.DocArg{},
			Return: "string",
			Text:   `__lisp-implementation-version__ returns a string describing the lisp implementation version.`,
			Examples: []string{
				`(lisp-implementation-version) => "v1.0.0"`,
			},
		}, &slip.CLPkg)
}

// LispImplementationVersion represents the lisp-implementation-version function.
type LispImplementationVersion struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *LispImplementationVersion) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 0)
	if len(version) == 0 {
		if bi, _ := debug.ReadBuildInfo(); bi != nil {
			version = bi.Main.Version
		}
	}
	return slip.String(version)
}
