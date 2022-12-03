// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

const (
	bold = "\x1b[1m"

	printANSI   = "*print-ansi*"
	stdInput    = "*standard-input*"
	stdOutput   = "*standard-output*"
	p1Key       = "*repl-prompt*"
	p1ANSIKey   = "*repl-prompt-ansi*"
	warnANSIKey = "*repl-warning-ansi*"
	form1Key    = "+"
	form2Key    = "++"
	form3Key    = "+++"
	value1Key   = "/"
	value2Key   = "//"
	value3Key   = "///"
)

type REPL struct {
	flavors.Instance

	// TBD is a lock file needed to avoid collisions when running multiple
	// instances?
	configFilename  string
	historyFilename string

	line  []byte
	buf   []byte
	depth int

	form1 slip.Object
	form2 slip.Object
	form3 slip.Object

	value1 slip.Object
	value2 slip.Object
	value3 slip.Object
}

// NewREPL returns a new instance or a REPL.
func NewREPL() *REPL {
	r := REPL{line: make([]byte, 1024)}
	if r.Get(slip.Symbol(printANSI)) != nil {
		r.Let(slip.Symbol(p1ANSIKey), slip.String(bold))
		r.Let(slip.Symbol(warnANSIKey), slip.String("\x1b[31m"))
		r.Let(slip.Symbol(p1Key), slip.String("\x1b[1;94m▶ \x1b[m"))
	} else {
		r.Let(slip.Symbol(p1ANSIKey), nil)
		r.Let(slip.Symbol(warnANSIKey), nil)
		r.Let(slip.Symbol(p1Key), slip.String("* "))
	}
	r.Let(slip.Symbol(form1Key), nil)
	r.Let(slip.Symbol(form2Key), nil)
	r.Let(slip.Symbol(form3Key), nil)
	r.Let(slip.Symbol(value1Key), nil)
	r.Let(slip.Symbol(value2Key), nil)
	r.Let(slip.Symbol(value3Key), nil)

	return &r
}

// SetCfgDir sets the configuration directory for the configuration and
// history files.
func (r *REPL) SetCfgDir(dir string) {
	if strings.Contains(dir, "~") {
		home, err := os.UserHomeDir()
		if err != nil {
			panic(err)
		}
		dir = strings.ReplaceAll(dir, "~", home)
	}
	var err error
	if dir, err = filepath.Abs(filepath.Clean(dir)); err != nil {
		panic(err)
	}
	r.configFilename = filepath.Join(dir, "config.lisp")
	r.historyFilename = filepath.Join(dir, "history")

	os.MkdirAll(dir, 0755)
	var buf []byte
	if buf, err = os.ReadFile(r.configFilename); err == nil {
		code := slip.Read(buf)
		code.Compile()
		code.Eval(&r.Instance.Scope)
	} else if os.IsNotExist(err) {
		if err = os.WriteFile(r.configFilename, []byte(";;;; slip REPL configuration file\n"), 0666); err != nil {
			panic(err)
		}
	} else {
		panic(err)
	}
}

func (r *REPL) Run() {
	defer func() {
		_ = recover()
		_, _ = r.Get(slip.Symbol(stdOutput)).(io.Writer).Write([]byte("\nBye\n"))
	}()
	for {
		r.process()
	}
}

func (r *REPL) reset() {
	r.buf = r.buf[:0]
	r.depth = 0
}

func (r *REPL) process() {
	defer func() {
		rec := recover()
		if rec == nil {
			r.reset()
			return
		}
		var (
			prefix string
			suffix string
		)
		if r.Get(slip.Symbol(printANSI)) != nil {
			if ansi := r.Get(slip.Symbol(warnANSIKey)); ansi != nil {
				s, _ := ansi.(slip.String)
				prefix = string(s)
				suffix = "\x1b[m"
			}
		}
		switch tr := rec.(type) {
		case *slip.Partial:
			r.depth = tr.Depth
		case *slip.Panic:
			var buf []byte
			buf = append(buf, prefix...)
			buf = append(buf, tr.Bytes()...)
			buf = append(buf, suffix...)
			_, _ = r.Get(slip.Symbol(stdOutput)).(io.Writer).Write(buf)
			r.reset()
		case error:
			if errors.Is(tr, io.EOF) {
				panic(nil) // exits the REPL loop
			}
			fmt.Fprintf(r.Get(slip.Symbol(stdOutput)).(io.Writer), "%s## %v%s\n", prefix, tr, suffix)
			r.reset()
		default:
			fmt.Fprintf(r.Get(slip.Symbol(stdOutput)).(io.Writer), "%s## %v%s\n", prefix, tr, suffix)
			r.reset()
		}
	}()
	r.read()
	code := slip.Read(r.buf)
	for _, obj := range code {
		var skipWrite bool

		r.form3 = r.form2
		r.form2 = r.form1
		r.form1 = obj

		r.value3 = r.value2
		r.value2 = r.value1
		r.value1 = obj.Eval(&r.Instance.Scope, 0)
		if r.value1 == slip.Novalue {
			r.value1 = nil
			skipWrite = true
		}
		// Eval was successful.
		r.writeToHistory()

		r.Set(slip.Symbol(form1Key), r.form1)
		r.Set(slip.Symbol(form2Key), r.form2)
		r.Set(slip.Symbol(form3Key), r.form3)

		r.Set(slip.Symbol(value1Key), r.value1)
		r.Set(slip.Symbol(value2Key), r.value2)
		r.Set(slip.Symbol(value3Key), r.value3)

		if !skipWrite {
			fmt.Fprintf(r.Get(slip.Symbol(stdOutput)).(io.Writer), "%s\n", slip.ObjectString(r.value1))
		}
	}
}

func (r *REPL) read() {
	var prefix slip.String
	var suffix string
	if ansi := r.Get(slip.Symbol(p1ANSIKey)); ansi != nil {
		prefix, _ = ansi.(slip.String)
		suffix = "\x1b[m"
	}
	if 0 < len(r.buf) {
		fmt.Fprintf(r.Get(slip.Symbol(stdOutput)).(io.Writer), "%s%d] %s", string(prefix), r.depth, suffix)
	} else {
		p1 := r.Get(slip.Symbol(p1Key)).(slip.String)
		fmt.Fprintf(r.Get(slip.Symbol(stdOutput)).(io.Writer), "%s%s%s", string(prefix), string(p1), suffix)
	}
	for {
		n, err := r.Get(slip.Symbol(stdInput)).(io.Reader).Read(r.line)
		if err != nil {
			panic(err)
		}
		r.buf = append(r.buf, r.line[:n]...)
		if 0 < len(r.buf) && r.buf[len(r.buf)-1] == '\n' {
			break
		}
	}
}

func (r *REPL) updateConfigFile() {
	// TBD write all repl vars as well as all *print-xxx* vars
	//   other global vars?
}

func (r *REPL) writeToHistory() {
	// TBD write r.buf to history
}
