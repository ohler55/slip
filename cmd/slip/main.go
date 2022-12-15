// Copyright (c) 2022, Peter Ohler, All rights reserved.

// Package main is the main package.
package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl"
	"golang.org/x/term"

	// Pull in all functions.
	_ "github.com/ohler55/slip/pkg"
)

var (
	version = "unknown"
	built   = "unknown"

	showVersion bool
	cfgDir      = "~/.slip"
	evalCode    string
	interactive bool
	trace       bool
)

func init() {
	flag.BoolVar(&showVersion, "v", showVersion, "version")
	flag.BoolVar(&trace, "t", trace, "trace")
	flag.StringVar(&evalCode, "e", evalCode, "code to evaluate")
	flag.StringVar(&cfgDir, "c", cfgDir, "configuration directory (an empty string or - indicates none)")
	flag.BoolVar(&interactive, "i", interactive, "interactive mode")
}

func main() {
	flag.Usage = func() {
		fmt.Fprintf(os.Stdout, `

usage: %s [<options>] [<filepath>]...

`, filepath.Base(os.Args[0]))
		flag.PrintDefaults()
		fmt.Fprintln(os.Stderr)
	}
	flag.Parse()
	if w, _, err := term.GetSize(0); err == nil {
		slip.CurrentPackage.Set("*print-right-margin*", slip.Fixnum(w-2))
	}
	run()
}

func run() {
	var path string
	defer func() {
		switch tr := recover().(type) {
		case nil:
			// normal exit
		case *slip.Panic:
			if slip.CurrentPackage.JustGet("*print-ansi*") != nil {
				_, _ = os.Stdout.Write(tr.Bytes())
			} else {
				buf := tr.Bytes()
				buf = append([]byte("\x1b[31m"), buf...)
				buf = append(buf, "\x1b[m"...)
				_, _ = os.Stdout.Write(buf)
			}
		default:
			if 0 < len(path) {
				fmt.Printf("\n## error: %s in %s\n\n", tr, path)
			} else {
				fmt.Printf("\n## error: %s\n\n", tr)
			}
		}
	}()
	if showVersion {
		fmt.Printf("slip version %s built on %s\n", version, built)
	}
	if trace {
		slip.Trace(true)
	}
	var scope *slip.Scope
	if 0 < len(evalCode) && !interactive {
		scope = slip.NewScope()
	} else {
		scope = repl.Scope()
		repl.ZeroMods()
	}
	var code slip.Code
	for _, path = range flag.Args() {
		if buf, err := os.ReadFile(path); err == nil {
			code = append(code, slip.Read(buf)...)
		} else {
			panic(err)
		}
	}
	path = "" // not loading a file so zero out the path
	code.Compile()
	code.Eval(scope)
	if 0 < len(evalCode) {
		code = slip.ReadString(evalCode)
		for _, obj := range code {
			obj.Eval(scope, 0)
		}
		if !interactive {
			return
		}
	}
	if 0 < len(cfgDir) && cfgDir != "-" {
		path = cfgDir
		repl.SetConfigDir(cfgDir)
		path = ""
	}
	repl.Run()
}
