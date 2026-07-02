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
	"strings"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/pkg/repl"
	"github.com/ohler55/slip/pkg/swank"
	"golang.org/x/term"

	// Pull in all slip functions.
	_ "github.com/ohler55/slip/pkg"
)

var (
	version = ""

	showVersion    bool
	cfgDir         string
	evalCode       string
	interactive    bool
	interactiveSet bool
	trace          bool
	allAtOnce      bool
	args           slip.List
	emacsMode      string
	coverage       string
)

func init() {
	cfgDir = repl.FindConfigDir()
	flag.BoolVar(&showVersion, "v", showVersion, "version")
	flag.BoolVar(&trace, "t", trace, "trace")
	flag.BoolVar(&repl.DebugEditor, "debug", repl.DebugEditor, "log each keypress to editor.log")
	flag.StringVar(&evalCode, "e", evalCode, "code to evaluate")
	flag.StringVar(&cfgDir, "c", cfgDir, "configuration directory (an empty string or - indicates none)")
	flag.BoolFunc("i", "interactive mode", func(v string) error {
		interactiveSet = true
		switch v {
		case "true":
			interactive = true
		case "false":
			interactive = false
		default:
			return fmt.Errorf("not a valid value for -i")
		}
		return nil
	})
	flag.BoolVar(&allAtOnce, "a", allAtOnce, "load all files at once instead of one by one")
	flag.Func("b", "bind the argument $<n> and add to the $@ list",
		func(s string) error {
			args = append(args, slip.String(s))
			return nil
		})
	flag.StringVar(&emacsMode, "emacs", "", "start Emacs integration server (slime|swank)")
	flag.BoolVar(&slip.Provenance, "p", false, "turn on provenance tracking")
	flag.StringVar(&coverage, "cover", "", "save coverage to provided file")
}

func main() {
	flag.Usage = func() {
		_, _ = fmt.Fprintf(os.Stderr, `
%s%s%s version %s

A Common LISP (mostly) evaluator and REPL with support for snapshots, stashing,
history, tab completion, and multiple help options..

usage: %[2]s [<options>] [<filepath>]...

`, "\x1b[1m", filepath.Base(os.Args[0]), "\x1b[m", version)
		flag.PrintDefaults()
		fmt.Fprintln(os.Stderr)
	}
	// Handle bare -emacs flag (without value) by setting default
	for i, arg := range os.Args {
		if arg == "-emacs" {
			// Check if next arg exists and is not another flag
			if i+1 >= len(os.Args) || strings.HasPrefix(os.Args[i+1], "-") {
				// Insert default value
				os.Args = append(os.Args[:i+1], append([]string{"slime"}, os.Args[i+1:]...)...)
				break
			}
		}
	}
	flag.Parse()
	// Leave as the default or what ever the user has in their defaults if
	// term does give a width.
	if w, _, err := term.GetSize(0); err == nil && 2 < w {
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
	slip.CLPkg.Locked = false // a bit of a cheat
	_ = slip.CLPkg.DefConst("*config-directory*", slip.String(cfgDir), "Config directory")
	slip.CLPkg.Locked = true
	slip.CLPkg.Export("*config-directory*")

	// Start Emacs integration server if requested
	if emacsMode != "" {
		startEmacsServer()
	}
	if 0 < len(coverage) {
		slip.StartCoverage()
	}
	run()
	if 0 < len(coverage) {
		slip.StopCoverage()
		slip.WriteCoverage(coverage)
	}
}

// startEmacsServer starts the appropriate Emacs integration server.
func startEmacsServer() {
	scope := slip.NewScope()
	mode := strings.ToLower(emacsMode)

	switch mode {
	case "slime", "swank":
		server := swank.NewServer(scope)
		if err := server.Start(":4005"); err != nil {
			fmt.Fprintf(os.Stderr, "Failed to start swank server: %v\n", err)
			os.Exit(1)
		}
		fmt.Printf("Swank server started on %s (for SLIME)\n", server.Addr())
	default:
		fmt.Fprintf(os.Stderr, "Unknown emacs mode: %s (use 'slime')\n", emacsMode)
		os.Exit(1)
	}
}

func run() {
	var (
		path  string
		scope *slip.Scope
	)
	defer func() {
		switch tr := recover().(type) {
		case nil:
			// normal exit
		case *slip.Panic:
			var (
				prefix string
				suffix string
			)
			if slip.CurrentPackage.JustGet("*print-ansi*") == nil {
				_, _ = fmt.Printf("\n## error: %s\n\n", tr.Error())
			} else {
				_, _ = fmt.Printf("\n\x1b[31m## error: %s\x1b[m\n", tr.Error())
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
		repl.Stop()
	}()
	if trace {
		repl.Trace = true
		slip.Trace(slip.List{slip.True})
	}
	if 0 < len(evalCode) && !interactive {
		scope = slip.NewScope()
	} else {
		scope = repl.Scope()
		repl.ZeroMods()
		if cfgDir != "-" {
			if 0 < len(cfgDir) {
				path = cfgDir // for defer panic handler
				repl.SetConfigDir(cfgDir)
				path = ""
			} else {
				cfgDir = repl.FindConfigDir()
				path = cfgDir // for defer panic handler
				repl.SetConfigDir(cfgDir)
				path = ""
			}
		}
	}
	bag.SetCompileScript(scope)
	var (
		code slip.Code
		w    io.Writer
	)
	scope.Let(slip.Symbol("$@"), args)
	scope.Let(slip.Symbol("$0"), slip.String(os.Args[0]))
	for i, a := range args {
		scope.Let(slip.Symbol(fmt.Sprintf("$%d", i+1)), a)
	}
	verbose := scope.Get(slip.Symbol("*load-verbose*"))
	print := scope.Get(slip.Symbol("*load-print*"))
	if verbose != nil || print != nil {
		w, _ = scope.Get("*standard-output*").(io.Writer)
	}
	if interactive || len(evalCode) == 0 {
		repl.Interactive = true
	}
	var listProvs slip.ProvSet
	if allAtOnce {
		var paths slip.List
		for _, path = range flag.Args() {
			if buf, err := os.ReadFile(path); err == nil {
				if !filepath.IsAbs(path) {
					path = filepath.Join(slip.WorkingDir, path)
				}
				if w != nil {
					_, _ = fmt.Fprintf(w, ";; Loading contents of %s\n", path)
				}
				var c slip.Code
				c, listProvs = slip.ReadProv(buf, scope, path, listProvs)
				code = append(code, c...)
				paths = append(paths, slip.String(path))
			} else {
				panic(err)
			}
		}
		scope.UnsafeLet(slip.Symbol("*load-pathname*"), paths)
		scope.UnsafeLet(slip.Symbol("*load-truename*"), paths)
		code.CompileWithProvenance(listProvs)
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
				pathname := slip.String(path)
				if !filepath.IsAbs(path) {
					pathname = slip.String(filepath.Join(slip.WorkingDir, path))
				}
				scope.UnsafeLet(slip.Symbol("*load-pathname*"), pathname)
				scope.UnsafeLet(slip.Symbol("*load-truename*"), pathname)
				if w != nil {
					_, _ = fmt.Fprintf(w, ";; Loading contents of %s\n", pathname)
				}
				code, listProvs = slip.ReadProv(buf, scope, string(pathname), listProvs)
				code.CompileWithProvenance(listProvs)
				var loaded slip.Object
				if print == nil {
					loaded = code.Eval(scope, nil)
				} else {
					loaded = code.Eval(scope, w)
				}
				if sys, ok := loaded.(*flavors.Instance); ok && sys.Class().Name() == "system" {
					sys.Set(slip.Symbol("pathname"), slip.String(filepath.Dir(path)))
					_ = sys.Receive(scope, ":fetch", nil, 0)
					_ = sys.Receive(scope, ":load", nil, 0)
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
		code = slip.ReadString(evalCode, scope)
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
	if !interactive && interactiveSet {
		return
	}
	repl.Run()
}
