// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"errors"
	"fmt"
	"io"
)

const (
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

type repl struct {
	line  []byte
	buf   []byte
	depth int
	scope *Scope

	form1 Object
	form2 Object
	form3 Object

	value1 Object
	value2 Object
	value3 Object
}

// REPL is a Read Eval Print Loop.
func REPL(scope ...*Scope) {
	r := repl{line: make([]byte, 1024)}
	if 0 < len(scope) {
		r.scope = scope[0]
	} else {
		r.scope = NewScope()
	}
	if r.scope.get(printANSI) != nil {
		r.scope.Let(Symbol(p1ANSIKey), String(bold))
		r.scope.Let(Symbol(warnANSIKey), String("\x1b[31m"))
		r.scope.Let(Symbol(p1Key), String("\x1b[1;94m▶ \x1b[m"))
	} else {
		r.scope.Let(Symbol(p1ANSIKey), nil)
		r.scope.Let(Symbol(warnANSIKey), nil)
		r.scope.Let(Symbol(p1Key), String("* "))
	}
	r.scope.Let(Symbol(form1Key), nil)
	r.scope.Let(Symbol(form2Key), nil)
	r.scope.Let(Symbol(form3Key), nil)
	r.scope.Let(Symbol(value1Key), nil)
	r.scope.Let(Symbol(value2Key), nil)
	r.scope.Let(Symbol(value3Key), nil)
	defer func() {
		_ = recover()
		_, _ = r.scope.get(stdOutput).(io.Writer).Write([]byte("\nBye\n"))
	}()
	for {
		r.process()
	}
}

func (r *repl) reset() {
	r.buf = r.buf[:0]
	r.depth = 0
}

func (r *repl) process() {
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
		if r.scope.get(printANSI) != nil {
			if ansi := r.scope.get(warnANSIKey); ansi != nil {
				s, _ := ansi.(String)
				prefix = string(s)
				suffix = "\x1b[m"
			}
		}
		switch tr := rec.(type) {
		case *Partial:
			r.depth = tr.Depth
		case *Panic:
			var buf []byte
			buf = append(buf, prefix...)
			buf = append(buf, "## "...)
			buf = append(buf, tr.Message...)
			buf = append(buf, '\n')
			for _, line := range tr.Stack {
				buf = append(buf, "##  "...)
				buf = append(buf, line...)
				buf = append(buf, '\n')
			}
			buf = append(buf, suffix...)
			_, _ = r.scope.get(stdOutput).(io.Writer).Write(buf)
			r.reset()
		case error:
			if errors.Is(tr, io.EOF) {
				panic(nil) // exits the REPL loop
			}
			// Must be an internal error. Most likely an error on read or write.
			fmt.Fprintf(r.scope.get(stdOutput).(io.Writer), "%s## %v%s\n", prefix, tr, suffix)
			panic(tr)
		default:
			fmt.Fprintf(r.scope.get(stdOutput).(io.Writer), "%s## %v%s\n", prefix, tr, suffix)
			panic(tr)
		}
	}()
	r.read()
	code := Read(r.buf)
	for _, obj := range code {
		var skipWrite bool

		r.form3 = r.form2
		r.form2 = r.form1
		r.form1 = obj

		r.value3 = r.value2
		r.value2 = r.value1
		r.value1 = obj.Eval(r.scope, 0)
		if r.value1 == Novalue {
			r.value1 = nil
			skipWrite = true
		}

		r.scope.set(form1Key, r.form1)
		r.scope.set(form2Key, r.form2)
		r.scope.set(form3Key, r.form3)

		r.scope.set(value1Key, r.value1)
		r.scope.set(value2Key, r.value2)
		r.scope.set(value3Key, r.value3)

		if !skipWrite {
			fmt.Fprintf(r.scope.get(stdOutput).(io.Writer), "%s\n", ObjectString(r.value1))
		}
	}
}

func (r *repl) read() {
	var prefix String
	var suffix string
	if ansi := r.scope.get(p1ANSIKey); ansi != nil {
		prefix, _ = ansi.(String)
		suffix = "\x1b[m"
	}
	if 0 < len(r.buf) {
		fmt.Fprintf(r.scope.get(stdOutput).(io.Writer), "%s%d] %s", string(prefix), r.depth, suffix)
	} else {
		p1 := r.scope.get(p1Key).(String)
		fmt.Fprintf(r.scope.get(stdOutput).(io.Writer), "%s%s%s", string(prefix), string(p1), suffix)
	}
	for {
		n, err := r.scope.get(stdInput).(io.Reader).Read(r.line)
		if err != nil {
			panic(err)
		}
		r.buf = append(r.buf, r.line[:n]...)
		if 0 < len(r.buf) && r.buf[len(r.buf)-1] == '\n' {
			break
		}
	}
}
