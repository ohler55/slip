// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReadLineStdin(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.NewInputStream(strings.NewReader("𝄢bc"))
	scope.Let(slip.Symbol("*standard-input*"), stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-line)`,
		Expect: `"𝄢bc", t`,
	}).Test(t)
}

func TestReadLineStream(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.NewInputStream(strings.NewReader("abc\n"))
	scope.Let(slip.Symbol("in"), stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-line in)`,
		Expect: `"abc", nil`,
	}).Test(t)
}

func TestReadLineEOFPanic(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.NewInputStream(strings.NewReader(""))
	scope.Let(slip.Symbol("in"), stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-line in)`,
		Panics: true,
	}).Test(t)
}

func TestReadLineEOFNil(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.NewInputStream(strings.NewReader(""))
	scope.Let(slip.Symbol("in"), stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-line in nil)`,
		Expect: `nil, t`,
	}).Test(t)
}

func TestReadLineEOFNonNil(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.NewInputStream(strings.NewReader(""))
	scope.Let(slip.Symbol("in"), stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-line in nil 'done)`,
		Expect: `done, t`,
	}).Test(t)
}

func TestReadLineNotStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-line t)`,
		Panics: true,
	}).Test(t)
}
