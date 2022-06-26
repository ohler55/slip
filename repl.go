// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"errors"
	"fmt"
	"io"
)

const (
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
	if printer.ANSI {
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
		_, _ = (StandardOutput.(io.Writer)).Write([]byte("\nBye\n"))
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
		switch tr := recover().(type) {
		case nil:
			r.reset()
		case *Partial:
			r.depth = tr.Depth
		case *Panic:
			var buf []byte

			if printer.ANSI {
				var prefix String
				if ansi := r.scope.get(warnANSIKey); ansi != nil {
					prefix, _ = ansi.(String)
					buf = append(buf, []byte(prefix)...)
				}
			}
			buf = append(buf, "## "...)
			buf = append(buf, tr.Message...)
			buf = append(buf, '\n')
			for _, line := range tr.Stack {
				buf = append(buf, "##  "...)
				buf = append(buf, line...)
				buf = append(buf, '\n')
			}
			if printer.ANSI {
				buf = append(buf, colorOff...)
			}
			_, _ = (StandardOutput.(io.Writer)).Write(buf)
			r.reset()
		case error:
			if errors.Is(tr, io.EOF) {
				panic(nil) // exits the REPL loop
			}
			r.printWarning(tr)
			r.reset()
		default:
			r.printWarning(tr)
			r.reset()
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
			fmt.Fprintf(StandardOutput.(io.Writer), "%s\n", ObjectString(r.value1))
		}
	}
}

func (r *repl) printWarning(val interface{}) {
	if printer.ANSI {
		var prefix String
		if ansi := r.scope.get(warnANSIKey); ansi != nil {
			prefix, _ = ansi.(String)
		}
		fmt.Fprintf(StandardOutput.(io.Writer), "%s## %v\x1b[m\n", string(prefix), val)
	} else {
		fmt.Fprintf(StandardOutput.(io.Writer), "## %v\n", val)
	}
}

func (r *repl) read() {
	if printer.ANSI {
		var prefix String
		if ansi := r.scope.get(p1ANSIKey); ansi != nil {
			prefix, _ = ansi.(String)
		}
		if 0 < len(r.buf) {
			fmt.Fprintf(StandardOutput.(io.Writer), "%s%d] \x1b[m", string(prefix), r.depth)
		} else {
			p1 := r.scope.get(p1Key).(String)
			fmt.Fprintf(StandardOutput.(io.Writer), "%s%s\x1b[m", string(prefix), string(p1))
		}
	} else {
		if 0 < len(r.buf) {
			fmt.Fprintf(StandardOutput.(io.Writer), "%d] ", r.depth)
		} else {
			p1 := r.scope.get(p1Key).(String)
			_, _ = (StandardOutput.(io.Writer)).Write([]byte(p1))
		}
	}
	for {
		n, err := (StandardInput.(io.Reader)).Read(r.line)
		if err != nil {
			panic(err)
		}
		r.buf = append(r.buf, r.line[:n]...)
		if 0 < len(r.buf) && r.buf[len(r.buf)-1] == '\n' {
			break
		}
	}
}
