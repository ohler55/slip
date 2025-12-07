// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReadCharStdin(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.NewInputStream(strings.NewReader("𝄢bc"))
	scope.Let(slip.Symbol("*standard-input*"), stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-char)`,
		Expect: `#\𝄢`,
	}).Test(t)
}

func TestReadCharStream(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.NewInputStream(strings.NewReader("abc"))
	scope.Let(slip.Symbol("in"), stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-char in)`,
		Expect: `#\a`,
	}).Test(t)
}

func TestReadCharEOFPanic(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.NewInputStream(strings.NewReader(""))
	scope.Let(slip.Symbol("in"), stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-char in)`,
		Panics: true,
	}).Test(t)
}

func TestReadCharEOFNil(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.NewInputStream(strings.NewReader(""))
	scope.Let(slip.Symbol("in"), stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-char in nil)`,
		Expect: `nil`,
	}).Test(t)
}

func TestReadCharEOFNonNil(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.NewInputStream(strings.NewReader(""))
	scope.Let(slip.Symbol("in"), stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-char in nil 'done)`,
		Expect: `done`,
	}).Test(t)
}

func TestReadCharNotStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-char t)`,
		Panics: true,
	}).Test(t)
}
