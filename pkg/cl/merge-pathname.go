// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"path/filepath"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MergePathnames{Function: slip.Function{Name: "merge-pathnames", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "merge-pathnames",
			Args: []*slip.DocArg{
				{
					Name: "pathname",
					Type: "string",
					Text: "The pathname to merge with default-pathname.",
				},
				{Name: "&optional"},
				{
					Name: "default-pathname",
					Type: "string",
					Text: "The default pathname to merge with. The default is _*default-pathname-defaults*_.",
				},
				{
					Name: "default-version",
					Type: "object",
					Text: "Not supported.",
				},
			},
			Return: "list",
			Text:   `__merge-pathnames__ joins the _default-pathname_ with _pathname_ to form a filepath.`,
			Examples: []string{
				`(merge-pathnames "three.lisp" "one/two/") => "one/two/three.lisp"`,
			},
		}, &slip.CLPkg)
}

// MergePathnames represents the merge-pathnames function.
type MergePathnames struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MergePathnames) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 3)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("pathname", args[0], "string")
	}
	dir := s.Get(slip.Symbol("*default-pathname-defaults*")).(slip.String)
	if 1 < len(args) {
		if sa, ok := args[1].(slip.String); ok {
			dir = sa
		} else {
			slip.PanicType("default-pathname", args[1], "string")
		}
	}
	return slip.String(filepath.Join(string(dir), string(path)))
}
