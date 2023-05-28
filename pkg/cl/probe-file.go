// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"os"
	"path/filepath"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ProbeFile{Function: slip.Function{Name: "probe-file", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "probe-file",
			Args: []*slip.DocArg{
				{
					Name: "path",
					Type: "string",
					Text: "The filepath to probe for existence.",
				},
			},
			Return: "string|nil",
			Text:   `__probe-file__ returns the full path to the file if it exists or _nil_ if it does not.`,
			Examples: []string{
				`(probe-file "foo.lisp") => "/top/one/foo.lisp"`,
			},
		}, &slip.CLPkg)
}

// ProbeFile represents the probe-file function.
type ProbeFile struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ProbeFile) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("string", args[0], "string")
	}
	if _, err := os.Stat(string(path)); err != nil {
		return nil
	}
	spath, _ := filepath.Abs(string(path))

	return slip.String(spath)
}
