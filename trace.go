// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"io"
	"sort"
	"strconv"
)

const warnANSIKey = "*repl-warning-ansi*"

var (
	beforeEval = noopBefore
	afterEval  = normalAfter
	traceFuncs map[string]bool
)

// Trace turns tracing on or off for the scope and any future sub-scopes.
func Trace(args List) (names List) {
	switch {
	case len(args) == 0:
		if traceFuncs != nil {
			keys := make([]string, 0, len(traceFuncs))
			for k := range traceFuncs {
				keys = append(keys, k)
			}
			sort.Strings(keys)
			names = make(List, len(keys))
			for i, k := range keys {
				names[i] = Symbol(k)
			}
		} else {
			names = List{True}
		}
	case len(args) == 1 && args[0] == True:
		beforeEval = traceBefore
		afterEval = traceAfter
	default:
		if traceFuncs == nil {
			traceFuncs = map[string]bool{}
		}
		for _, a := range args {
			traceFuncs[MustBeString(a, "trace")] = true
		}
		beforeEval = traceSelectedBefore
		afterEval = traceSelectedAfter
	}
	return
}

// Untrace turns tracing off for the scope and any future sub-scopes. If no
// arguments then tracing is turned off for all else just for the specified
// functions.
func Untrace(args List) {
	if 0 < len(args) {
		if traceFuncs != nil {
			for _, a := range args {
				delete(traceFuncs, MustBeString(a, "untrace"))
			}
		}
	} else {
		beforeEval = noopBefore
		afterEval = normalAfter
	}
}

func noopBefore(s *Scope, fn *Function, depth int) {
}

func normalAfter(s *Scope, fn *Function, depth int, result *Object) {
	switch tr := recover().(type) {
	case nil:
	case *Panic:
		tr.AppendToStack(fn)
		panic(tr)
	case Instance:
		panic(WrapError(s, tr, fn))
	default:
		cond := ErrorNew(s, depth, "%s", tr).(Instance)
		_ = cond.SetSlotValue(Symbol("stack"), List{fn})
		p := WrapError(s, cond, fn)
		p.Value = SimpleObject(tr)
		panic(p)
	}
}

func traceBefore(s *Scope, fn *Function, depth int) {
	var b []byte

	if len(indentSpaces)/2 <= depth {
		b = append(b, indentSpaces...)
	} else {
		b = append(b, []byte(indentSpaces)[:depth*2]...)
	}
	b = strconv.AppendInt(b, int64(depth), 10)
	b = append(b, ": "...)
	b = ObjectAppend(b, fn)
	b = append(b, '\n')
	if w, _ := s.Get(Symbol("*trace-output*")).(io.Writer); w != nil {
		_, _ = w.Write(b)
	}
}

func traceAfter(s *Scope, fn *Function, depth int, result *Object) {
	var b []byte

	if len(indentSpaces)/2 <= depth {
		b = append(b, indentSpaces...)
	} else {
		b = append(b, []byte(indentSpaces)[:depth*2]...)
	}
	b = strconv.AppendInt(b, int64(depth), 10)
	b = append(b, ": "...)
	b = ObjectAppend(b, fn)
	b = append(b, " => "...)

	switch tr := recover().(type) {
	case nil:
		b = ObjectAppend(b, *result)
		b = append(b, '\n')
		if w, _ := s.Get(Symbol("*trace-output*")).(io.Writer); w != nil {
			_, _ = w.Write(b)
		}
	case *Panic:
		tr.AppendToStack(fn)
		traceWriterPanic(s, b, tr)
		panic(tr)
	case Instance:
		p := WrapError(s, tr, fn)
		traceWriterPanic(s, b, p)
		panic(WrapError(s, tr, fn))
	default:
		cond := ErrorNew(s, depth, "%s", tr).(Instance)
		_ = cond.SetSlotValue(Symbol("stack"), append(List{Symbol(fn.Name)}, fn.Args...))
		p := WrapError(s, cond, fn)
		p.Value = SimpleObject(tr)
		traceWriterPanic(s, b, p)
		panic(p)
	}
}

func traceSelectedBefore(s *Scope, fn *Function, depth int) {
	if traceFuncs[fn.Name] {
		traceBefore(s, fn, depth)
	}
}

func traceSelectedAfter(s *Scope, fn *Function, depth int, result *Object) {
	if traceFuncs[fn.Name] {
		traceAfter(s, fn, depth, result)
	}
}

func traceWriterPanic(s *Scope, b []byte, p *Panic) {
	if s.Get(Symbol("*print-ansi*")) != nil {
		color := "\x1b[31m"
		sym := Symbol(warnANSIKey)
		if s.Bound(sym) {
			if o, ok := s.Get(sym).(String); ok {
				color = string(o)
			}
		}
		b = append(b, color...)
		b = append(b, p.Error()...)
		b = append(b, colorOff...)
	} else {
		b = append(b, p.Error()...)
	}
	b = append(b, '\n')
	if w, _ := s.Get(Symbol("*trace-output*")).(io.Writer); w != nil {
		_, _ = w.Write(b)
	}
}
