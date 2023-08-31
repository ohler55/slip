// Copyright (c) 2022, Peter Ohler, All rights reserved.

// Package main is the main package.
package main

import (
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime/debug"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl"
	"golang.org/x/term"

	// Pull in all functions.
	_ "github.com/ohler55/slip/pkg"
)

var (
	version = ""

	showVersion bool
	cfgDir      = "~/.slip"
	evalCode    string
	interactive bool
	trace       bool
	allAtOnce   bool
)

func init() {
	flag.BoolVar(&showVersion, "v", showVersion, "version")
	flag.BoolVar(&trace, "t", trace, "trace")
	flag.StringVar(&evalCode, "e", evalCode, "code to evaluate")
	flag.StringVar(&cfgDir, "c", cfgDir, "configuration directory (an empty string or - indicates none)")
	flag.BoolVar(&interactive, "i", interactive, "interactive mode")
	flag.BoolVar(&allAtOnce, "a", allAtOnce, "load all files are once instead of one by one")
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
	if showVersion {
		if len(version) == 0 {
			if bi, _ := debug.ReadBuildInfo(); bi != nil {
				version = bi.Main.Version
			}
		}
		fmt.Printf("slip version: %s\n", version)
		return
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
				_, _ = os.Stdout.Write(tr.Append(nil))
			} else {
				buf := tr.Append(nil)
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
		repl.Stop()
	}()
	if trace {
		slip.Trace(true)
	}
	var scope *slip.Scope
	if 0 < len(evalCode) && !interactive {
		scope = slip.NewScope()
	} else {
		scope = repl.Scope()
		repl.ZeroMods()
		if 0 < len(cfgDir) && cfgDir != "-" {
			path = cfgDir // for defer panic handler
			repl.SetConfigDir(cfgDir)
			path = ""
		}
	}
	var (
		code slip.Code
		w    io.Writer
	)
	verbose := scope.Get(slip.Symbol("*load-verbose*"))
	print := scope.Get(slip.Symbol("*load-print*"))
	if verbose != nil || print != nil {
		w, _ = scope.Get("*standard-output*").(io.Writer)
	}
	if interactive || len(evalCode) == 0 {
		repl.Interactive = true
	}
	if allAtOnce {
		var paths slip.List
		for _, path = range flag.Args() {
			if buf, err := os.ReadFile(path); err == nil {
				path = filepath.Join(slip.WorkingDir, path)
				if w != nil {
					_, _ = fmt.Fprintf(w, ";; Loading contents of %s\n", path)
				}
				code = append(code, slip.Read(buf)...)
				paths = append(paths, slip.String(path))
			} else {
				panic(err)
			}
		}
		scope.UnsafeLet(slip.Symbol("*load-pathname*"), paths)
		scope.UnsafeLet(slip.Symbol("*load-truename*"), paths)
		code.Compile()
		if print == nil {
			code.Eval(scope, nil)
		} else {
			code.Eval(scope, w)
		}
		if w != nil {
			for _, p := range paths {
				_, _ = fmt.Fprintf(w, ";; Finished loading %s\n", p)
			}
		}
	} else {
		for _, path = range flag.Args() {
			if buf, err := os.ReadFile(path); err == nil {
				pathname := slip.String(filepath.Join(slip.WorkingDir, path))
				scope.UnsafeLet(slip.Symbol("*load-pathname*"), pathname)
				scope.UnsafeLet(slip.Symbol("*load-truename*"), pathname)
				if w != nil {
					_, _ = fmt.Fprintf(w, ";; Loading contents of %s\n", pathname)
				}
				code = slip.Read(buf)
				code.Compile()
				if print == nil {
					code.Eval(scope, nil)
				} else {
					code.Eval(scope, w)
				}
				if w != nil {
					_, _ = fmt.Fprintf(w, ";; Finished loading %s\n", pathname)
				}
			} else {
				panic(err)
			}
		}
	}
	scope.Remove(slip.Symbol("*load-pathname*"))
	scope.Remove(slip.Symbol("*load-truename*"))
	if 0 < len(evalCode) {
		path = ""
		code = slip.ReadString(evalCode)
		for _, obj := range code {
			result := obj.Eval(scope, 0)
			if print != nil {
				_, _ = fmt.Fprintf(w, ";;  %s\n", slip.ObjectString(result))
			}
		}
		if !interactive {
			return
		}
	}
	repl.Run()
}
