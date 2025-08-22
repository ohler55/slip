// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"io/fs"
	"os"
	"path/filepath"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Truename{Function: slip.Function{Name: "truename", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "truename",
			Args: []*slip.DocArg{
				{
					Name: "filepsec",
					Type: "pathname|file-stream|synonym-stream",
					Text: "A file specifier.",
				},
			},
			Return: "string",
			Text: `__truename__ returns absolute path to the designated file. An error is raised
if the file does not exist.`,
			Examples: []string{
				`(truename "~/quux.lisp") => "/Users/someone/quux.lisp"`,
				`(with-open-file (f "~/quux.lisp" :direction :input) (truename f)) => "/Users/someone/quux.lisp"`,
			},
		}, &slip.CLPkg)
}

// Truename represents the truename function.
type Truename struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Truename) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	arg := args[0]
	var (
		fi  fs.FileInfo
		err error
	)
top:
	switch ta := arg.(type) {
	case slip.String:
		fi, err = os.Stat(string(ta))
	case *slip.FileStream:
		fi, err = (*os.File)(ta).Stat()
	case *SynonymStream:
		value, has := slip.CurrentPackage.Get(string(ta.symbol))
		if !has {
			slip.PanicUnboundVariable(ta.symbol, "unbound")
		}
		arg = value
		goto top
	default:
		slip.TypePanic(s, depth, "filespec", ta, "string", "file-stream", "synonym-stream for a file-stream")
	}
	if err != nil {
		slip.PanicFile(arg, "%s", err)
	}
	abs, _ := filepath.Abs(fi.Name())

	return slip.String(abs)
}
