// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"path/filepath"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := PathnameName{Function: slip.Function{Name: "pathname-name", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "pathname-name",
			Args: []*slip.DocArg{
				{
					Name: "pathname",
					Type: "string",
					Text: "The pathname to get the name from.",
				},
				{Name: "&key"},
				{
					Name: "case",
					Type: "keyword",
					Text: "The case keyword value is ignored.",
				},
			},
			Return: "string",
			Text:   `__pathname-name__ returns the base name of the _pathname_ excluding any extension.`,
			Examples: []string{
				`(pathname-name "../one/two/three.lisp") => "three"`,
			},
		}, &slip.CLPkg)
}

// PathnameName represents the pathname-name function.
type PathnameName struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *PathnameName) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 3)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("string", args[0], "string")
	}
	base := filepath.Base(string(path))
	if i := strings.LastIndexByte(base, '.'); 0 <= i {
		base = base[:i]
	}
	return slip.String(base)
}
