// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"io"
	"strconv"
)

const warnANSIKey = "*repl-warning-ansi*"

var (
	beforeEval = noopBefore
	afterEval  = normalAfter
)

// Trace turns tracing on or off for the scope and any future sub-scopes.
func Trace(on bool) {
	if on {
		beforeEval = traceBefore
		afterEval = traceAfter
	} else {
		beforeEval = noopBefore
		afterEval = normalAfter
	}
}

func noopBefore(s *Scope, name string, args List, depth int) {
}

func normalAfter(s *Scope, name string, args List, depth int, result *Object) {
	switch tr := recover().(type) {
	case nil:
	case *Panic:
		tr.Stack = append(tr.Stack, ObjectString(append(List{Symbol(name)}, args...)))
		panic(tr)
	default:
		panic(&Panic{
			Message: fmt.Sprint(tr), Stack: []string{ObjectString(append(List{Symbol(name)}, args...))},
			Value: SimpleObject(tr),
		})
	}
}

func traceBefore(s *Scope, name string, args List, depth int) {
	var b []byte

	if len(indentSpaces)/2 <= depth {
		b = append(b, indentSpaces...)
	} else {
		b = append(b, []byte(indentSpaces)[:depth*2]...)
	}
	b = strconv.AppendInt(b, int64(depth), 10)
	b = append(b, ": "...)
	b = ObjectAppend(b, append(List{Symbol(name)}, args...))
	b = append(b, '\n')
	_, _ = StandardOutput.(io.Writer).Write(b)
}

func traceAfter(s *Scope, name string, args List, depth int, result *Object) {
	var b []byte

	if len(indentSpaces)/2 <= depth {
		b = append(b, indentSpaces...)
	} else {
		b = append(b, []byte(indentSpaces)[:depth*2]...)
	}
	b = strconv.AppendInt(b, int64(depth), 10)
	b = append(b, ": "...)
	b = ObjectAppend(b, append(List{Symbol(name)}, args...))
	b = append(b, " => "...)

	switch tr := recover().(type) {
	case nil:
		b = ObjectAppend(b, *result)
		b = append(b, '\n')
		_, _ = StandardOutput.(io.Writer).Write(b)
	case *Panic:
		tr.Stack = append(tr.Stack, ObjectString(append(List{Symbol(name)}, args...)))
		traceWriterPanic(s, b, tr)
		panic(tr)
	default:
		p := Panic{
			Message: fmt.Sprint(tr), Stack: []string{ObjectString(append(List{Symbol(name)}, args...))},
		}
		traceWriterPanic(s, b, &p)
		panic(&p)
	}
}

func traceWriterPanic(s *Scope, b []byte, p *Panic) {
	if printer.ANSI {
		color := "\x1b[31m"
		sym := Symbol(warnANSIKey)
		if s.Bound(sym) {
			if o, ok := s.Get(sym).(String); ok {
				color = string(o)
			}
		}
		b = append(b, color...)
		b = append(b, p.Message...)
		b = append(b, colorOff...)
	} else {
		b = append(b, p.Message...)
	}
	b = append(b, '\n')
	_, _ = StandardOutput.(io.Writer).Write(b)
}
