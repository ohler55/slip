// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"errors"
	"fmt"
	"io"
)

const (
	p1Key       = "*REPL-PROMPT*"
	p1ANSIKey   = "*REPL-PROMPT-ANSI*"
	warnANSIKey = "*REPL-WARNING-ANSI*"
	plusKey     = "+"
	plus2Key    = "++"
	plus3Key    = "+++"
	// colorOff    = "\x1b[m"
)

type repl struct {
	line  []byte
	buf   []byte
	depth int
	scope *Scope
	prev  Object
	prev2 Object
	prev3 Object
}

// REPL is a Read Eval Print Loop.
func REPL() {
	r := repl{line: make([]byte, 1024), scope: NewScope()}
	r.scope.Let(Symbol(p1Key), String("* "))
	if printer.ANSI {
		r.scope.Let(Symbol(p1ANSIKey), String("\x1b[1m"))
		r.scope.Let(Symbol(warnANSIKey), String("\x1b[31m"))
	} else {
		r.scope.Let(Symbol(p1ANSIKey), nil)
		r.scope.Let(Symbol(warnANSIKey), nil)
	}
	r.scope.Let(Symbol(plusKey), nil)
	r.scope.Let(Symbol(plus2Key), nil)
	r.scope.Let(Symbol(plus3Key), nil)
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
			// TBD display and zero out the line and buffer
			fmt.Printf("*** panic: %s\n", tr)
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
		r.prev3 = r.prev2
		r.prev2 = r.prev
		r.prev = obj.Eval(r.scope, 0)
		r.scope.set(plusKey, r.prev)
		r.scope.set(plus2Key, r.prev2)
		r.scope.set(plus3Key, r.prev3)
		fmt.Fprintf(StandardOutput.(io.Writer), "%s\n", r.prev)
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
