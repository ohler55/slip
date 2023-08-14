// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := EnsureDirectoriesExist{Function: slip.Function{Name: "ensure-directories-exist", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "ensure-directories-exist",
			Args: []*slip.DocArg{
				{
					Name: "path",
					Type: "string",
					Text: "The path to ensure exist.",
				},
				{Name: "&key"},
				{
					Name: "verbose",
					Type: "boolean",
					Text: "ignored.",
				},
				{
					Name: "permission",
					Type: "fixnum",
					Text: "The file permissions when creating a file. This is not a common LISP standard.",
				},
			},
			Return: "path,boolean",
			Text: `__ensure-directories-exist__ ensures directories on the path exist and
returns the full path and _t_. An optional argument is the permissions for any newly created
directories. The permissions option is not standard common LISP.`,
			Examples: []string{
				`(ensure-directories-exist "one/twp") => "/top/one/two", t`,
			},
		}, &slip.CLPkg)
}

// EnsureDirectoriesExist represents the ensure-directories-exist function.
type EnsureDirectoriesExist struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *EnsureDirectoriesExist) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 5)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("string", args[0], "string")
	}
	var perm fs.FileMode = 0755
	for pos := 1; pos < len(args); pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		if len(args)-1 <= pos {
			slip.NewPanic("%s missing an argument", sym)
		}
		val := args[pos+1]
		switch strings.ToLower(string(sym)) {
		case ":permission":
			var num slip.Fixnum
			if num, ok = val.(slip.Fixnum); ok {
				perm = fs.FileMode(num)
			} else {
				slip.PanicType(string(sym), val, "fixnum")
			}
		case ":verbose":
			// ignore
		default:
			slip.PanicType("keyword", sym, ":permission", ":verbose")
		}
	}
	if err := os.MkdirAll(string(path), perm); err != nil {
		slip.NewPanic("mkdir failed: %s", err)
	}
	spath, _ := filepath.Abs(string(path))

	return slip.Values{slip.String(spath), slip.True}
}
