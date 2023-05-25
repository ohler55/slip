// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReadByteStdin(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.InputStream{Reader: strings.NewReader("abc")}
	scope.Let(slip.Symbol("*standard-input*"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-byte)`,
		Expect: `97`,
	}).Test(t)
}

func TestReadByteStream(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.InputStream{Reader: strings.NewReader("abc")}
	scope.Let(slip.Symbol("in"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-byte in)`,
		Expect: `97`,
	}).Test(t)
}

func TestReadByteEOFPanic(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.InputStream{Reader: strings.NewReader("")}
	scope.Let(slip.Symbol("in"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-byte in)`,
		Panics: true,
	}).Test(t)
}

func TestReadByteEOFNil(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.InputStream{Reader: strings.NewReader("")}
	scope.Let(slip.Symbol("in"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-byte in nil)`,
		Expect: `nil`,
	}).Test(t)
}

func TestReadByteEOFNonNil(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.InputStream{Reader: strings.NewReader("")}
	scope.Let(slip.Symbol("in"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(read-byte in nil 'done)`,
		Expect: `done`,
	}).Test(t)
}

func TestReadByteNotStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-byte t)`,
		Panics: true,
	}).Test(t)
}
