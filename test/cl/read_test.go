// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"fmt"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

type badReader int

func (w badReader) Read([]byte) (int, error) {
	return 0, fmt.Errorf("oops")
}

func TestReadStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(read (make-string-input-stream " 123 "))`,
		Expect: `123`,
	}).Test(t)
}

func TestReadEOF(t *testing.T) {
	(&sliptest.Function{
		Source:    `(read (make-string-input-stream "(123 "))`,
		PanicType: slip.Symbol("parse-error"),
	}).Test(t)
}

func TestReadEOFValue(t *testing.T) {
	(&sliptest.Function{
		Source: `(read (make-string-input-stream "(123 ") nil 'eof-error)`,
		Expect: `eof-error`,
	}).Test(t)
}

func TestReadEmptyEOFValue(t *testing.T) {
	(&sliptest.Function{
		Source: `(read (make-string-input-stream "  ") nil 'eof-error)`,
		Expect: `eof-error`,
	}).Test(t)
}

func TestReadNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(read "123")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestReadEmpty(t *testing.T) {
	(&sliptest.Function{
		Source:    `(read (make-string-input-stream "  "))`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}

func TestReadError(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), &slip.InputStream{Reader: badReader(0)})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(read in)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
