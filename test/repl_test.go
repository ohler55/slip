// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"io"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

type lineReader struct {
	lines []string
	next  int
}

func (r *lineReader) Read(p []byte) (n int, err error) {
	if len(r.lines) <= r.next {
		return 0, io.EOF
	}
	line := []byte(r.lines[r.next])
	copy(p, line)
	p[len(line)] = '\n'
	r.next++
	return len(line) + 1, nil
}

func TestREPLSimple(t *testing.T) {
	testREPL(t, "(car '(1 2))", "1")
}

func TestREPLPartial(t *testing.T) {
	testREPL(t, "(car '(1\n2))", "2] 1")
}

func TestREPLWarn(t *testing.T) {
	testREPL(t, "(car 3)", `## argument to car must be a cons or list not 3, a fixnum.
##  (car 3)`)
}

func TestREPLError(t *testing.T) {
	// TBD function called should panic
	// TBD function called should panic with and error (maybe read a file that's not there)
}

func TestREPLNovalue(t *testing.T) {
	var scope slip.Scope
	scope.Let(slip.Symbol("none"), slip.Novalue)
	testREPL(t, "none", "* \nBye\n", &scope)
}

func TestREPLANSI(t *testing.T) {
	var (
		out strings.Builder
		in  = strings.NewReader("(car '(1 2))\n")
	)
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*print-ansi*"), slip.True)
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let(slip.Symbol("*standard-input*"), &slip.InputStream{Reader: in})

	slip.REPL(scope)
	tt.Equal(t, '\x1b', out.String()[0])
}

func TestREPLNewScope(t *testing.T) {
	var (
		out strings.Builder
		in  = strings.NewReader("(car '(1 2))\n")
	)
	ansiKey := slip.Symbol("*print-ansi*")
	ansiOrig, _ := slip.GetVar(ansiKey)

	outKey := slip.Symbol("*standard-output*")
	outOrig, _ := slip.GetVar(outKey)

	inKey := slip.Symbol("*standard-input*")
	inOrig, _ := slip.GetVar(inKey)

	defer func() {
		slip.SetVar(inKey, inOrig)
		slip.SetVar(outKey, outOrig)
		slip.SetVar(ansiKey, ansiOrig)
	}()
	slip.SetVar(outKey, &slip.OutputStream{Writer: &out})
	slip.SetVar(inKey, &slip.InputStream{Reader: in})
	slip.SetVar(ansiKey, nil)
	slip.REPL()
	tt.Equal(t, "* 1\n* \nBye\n", out.String())
}

func testREPL(t *testing.T, input, expect string, scopes ...*slip.Scope) {
	var (
		out strings.Builder
		in  = &lineReader{lines: strings.Split(input, "\n")}
	)
	var scope *slip.Scope
	if 0 < len(scopes) {
		scope = scopes[0]
	} else {
		scope = slip.NewScope()
	}
	scope.Let(slip.Symbol("*print-ansi*"), nil)
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let(slip.Symbol("*standard-input*"), &slip.InputStream{Reader: in})

	slip.REPL(scope)

	result := strings.TrimSuffix(out.String(), "\n* \nBye\n")
	result = strings.TrimPrefix(result, "* ")
	tt.Equal(t, expect, result)
}
