// Copyright (c) 2026, Peter Ohler, All rights reserved.

// Package main is the main package.
package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"

	// Pull in all slip functions.
	_ "github.com/ohler55/slip/pkg"
)

var version = ""

func main() {
	path := os.Args[1]
	var (
		prefix string
		suffix string
	)
	defer func() {
		switch tr := recover().(type) {
		case nil:
			// normal exit
		case *slip.Panic:
			if len(tr.Message) == 0 && tr.Fatal || tr.Value == slip.Symbol(":help") {
				break
			}
			if slip.CurrentPackage.JustGet("*print-ansi*") == nil {
				_, _ = fmt.Printf("\n## error: %s\n\n", tr)
			} else {
				_, _ = fmt.Printf("\n\x1b[31m## error: %s\x1b[m\n", tr)
				prefix = "\x1b[31m"
				suffix = "\x1b[m"
			}
			msg := tr.Error()
			if 0 < len(msg) {
				var buf []byte
				buf = append(buf, prefix...)
				buf = tr.AppendFull(buf)
				buf = append(buf, suffix...)
				buf = append(buf, '\n')
				fmt.Print(string(buf))
			}
		default:
			if 0 < len(path) {
				fmt.Printf("\n## error: %s in %s\n\n", tr, path)
			} else {
				fmt.Printf("\n## error: %s\n\n", tr)
			}
		}
	}()
	scope := slip.NewScope()
	_ = slip.CurrentPackage.DefConst("*slipr-version*", slip.String(version), "slipr version")

	bag.SetCompileScript(scope)
	if buf, err := os.ReadFile(path); err == nil {
		pathname := slip.String(filepath.Join(slip.WorkingDir, path))
		scope.UnsafeLet(slip.Symbol("*load-pathname*"), pathname)
		scope.UnsafeLet(slip.Symbol("*load-truename*"), pathname)
		code, listProvs := slip.ReadProv(buf, scope, string(pathname), nil)
		code.CompileWithProvenance(listProvs)
		code.Eval(scope, nil)
	} else {
		panic(err)
	}
	scope.Remove(slip.Symbol("*load-pathname*"))
	scope.Remove(slip.Symbol("*load-truename*"))
}
