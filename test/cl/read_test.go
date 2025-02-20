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

type badSeeker struct {
	slip.StringStream
	whence int
}

func (bs badSeeker) Seek(offset int64, whence int) (int64, error) {
	if whence == bs.whence {
		return 0, fmt.Errorf("oops")
	}
	return 0, nil
}

func TestReadStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(read (make-string-input-stream " 123 "))`,
		Expect: `123`,
	}).Test(t)
}

func TestReadStreamMoreSeeker(t *testing.T) {
	(&sliptest.Function{
		Source: `(read (make-string-input-stream "123"))`,
		Expect: `123`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((ss (make-string-input-stream "123 456")))
                  (list (read ss) (read ss)))`,
		Expect: `(123 456)`,
	}).Test(t)
}

func TestReadStreamMoreOne(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), &slip.InputStream{Reader: slip.NewStringStream([]byte("123 456"))})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(list (read in) (read in))`,
		Expect: `(123 456)`,
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

func TestReadNotObject(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), &slip.InputStream{Reader: slip.NewStringStream([]byte("###"))})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(read in)`,
		PanicType: slip.ParseErrorSymbol,
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

func TestReadSeekError0(t *testing.T) {
	scope := slip.NewScope()
	bs := badSeeker{whence: 0}
	_, _ = bs.Write([]byte{'x'})
	_, _ = bs.StringStream.Seek(0, 0)
	scope.Let(slip.Symbol("in"), &bs)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(read in)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestReadSeekError1(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), &badSeeker{whence: 1})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(read in)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
