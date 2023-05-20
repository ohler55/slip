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
			f := PathnameType{Function: slip.Function{Name: "pathname-type", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "pathname-type",
			Args: []*slip.DocArg{
				{
					Name: "pathname",
					Type: "string",
					Text: "The pathname to get the type from.",
				},
				{Name: "&key"},
				{
					Name: "case",
					Type: "keyword",
					Text: "The case keyword value is ignored.",
				},
			},
			Return: "string",
			Text:   `__pathname-type__ returns the type (extension) of the _pathname_.`,
			Examples: []string{
				`(pathname-type "../one/two/three.lisp") => "lisp"`,
			},
		}, &slip.CLPkg)
}

// PathnameType represents the pathname-type function.
type PathnameType struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *PathnameType) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 3)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("string", args[0], "string")
	}
	ext := strings.TrimLeft(filepath.Ext(string(path)), ".")
	if 0 < len(ext) {
		result = slip.String(ext)
	}
	return
}
