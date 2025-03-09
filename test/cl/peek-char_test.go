// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPeekCharStdin(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.InputStream{Reader: strings.NewReader("𝄢bc")}
	scope.Let(slip.Symbol("*standard-input*"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(peek-char)`,
		Expect: `#\𝄢`,
	}).Test(t)
}

func TestPeekCharStream(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.InputStream{Reader: strings.NewReader("abc")}
	scope.Let(slip.Symbol("in"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(peek-char nil in)`,
		Expect: `#\a`,
	}).Test(t)
	result := slip.ReadString("(read-char in)", scope).Eval(scope, nil)
	tt.Equal(t, slip.Character('a'), result)
}

func TestPeekCharSkipWhite(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.InputStream{Reader: strings.NewReader("\t ab")}
	scope.Let(slip.Symbol("in"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(peek-char t in)`,
		Expect: `#\a`,
	}).Test(t)
	result := slip.ReadString("(read-char in)", scope).Eval(scope, nil)
	tt.Equal(t, slip.Character('a'), result)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(peek-char t in)`,
		Expect: `#\b`,
	}).Test(t)
	result = slip.ReadString("(read-char in)", scope).Eval(scope, nil)
	tt.Equal(t, slip.Character('b'), result)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(peek-char t in nil)`,
		Expect: `nil`,
	}).Test(t)
}

func TestPeekCharTarget(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.InputStream{Reader: strings.NewReader("\t abc")}
	scope.Let(slip.Symbol("in"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(peek-char #\b in)`,
		Expect: `#\b`,
	}).Test(t)
	result := slip.ReadString("(read-char in)", scope).Eval(scope, nil)
	tt.Equal(t, slip.Character('b'), result)
}

func TestPeekCharEOFPanic(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.InputStream{Reader: strings.NewReader("")}
	scope.Let(slip.Symbol("in"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(peek-char nil in)`,
		Panics: true,
	}).Test(t)
}

func TestPeekCharEOFNil(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.InputStream{Reader: strings.NewReader("")}
	scope.Let(slip.Symbol("in"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(peek-char nil in nil)`,
		Expect: `nil`,
	}).Test(t)
}

func TestPeekCharEOFNonNil(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.InputStream{Reader: strings.NewReader("")}
	scope.Let(slip.Symbol("in"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(peek-char nil in nil 'done)`,
		Expect: `done`,
	}).Test(t)
}

func TestPeekCharBadPeekType(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.InputStream{Reader: strings.NewReader("abc")}
	scope.Let(slip.Symbol("in"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(peek-char 'x in)`,
		Panics: true,
	}).Test(t)
}

func TestPeekCharNotStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(peek-char nil t)`,
		Panics: true,
	}).Test(t)
}
