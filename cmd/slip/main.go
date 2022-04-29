// Copyright (c) 2022, Peter Ohler, All rights reserved.

package main

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"github.com/ohler55/slip"
	// Pull in all functions.
	_ "github.com/ohler55/slip/pkg"
)

var (
	version = "unknown"
	built   = "unknown"

	verbose     bool
	evalExpr    string
	ansiOff     bool
	interactive bool
)

func init() {
	flag.BoolVar(&verbose, "v", verbose, "verbose")
	flag.BoolVar(&ansiOff, "a", ansiOff, "no ANSI codes")
	flag.StringVar(&evalExpr, "e", evalExpr, "Sexpression to evaluate")
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
	run()
}

func run() {
	var path string
	defer func() {
		switch tr := recover().(type) {
		case nil:
			// normal exit
		case *slip.Panic:
			fmt.Println(string(bytes.Replace(tr.Bytes(), []byte("###"), []byte("\n### error:"), 1)))
		default:
			if 0 < len(path) {
				fmt.Printf("\n### error: %s in %s\n\n", tr, path)
			} else {
				fmt.Printf("\n### error: %s\n\n", tr)
			}
		}
	}()
	if verbose {
		fmt.Printf("slip version %s built on %s\n", version, built)
	}
	if ansiOff {
		// TBD
	}
	for _, path = range flag.Args() {
		if buf, err := os.ReadFile(path); err == nil {
			// TBD read and eval
			fmt.Printf("*** %s\n", buf)
		} else {
			panic(err)
		}
	}
	path = ""
	if 0 < len(evalExpr) {
		// TBD
		return
	}
	slip.REPL()
}
