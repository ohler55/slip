// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWriteSequenceString(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString(`(write-sequence "abcdef" out :start 1 :end 3)`, scope).Eval(scope, nil)

	tt.Equal(t, slip.String("abcdef"), result)
	tt.Equal(t, "bc", out.String())
}

func TestWriteSequenceList(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(write-sequence '(65 #\B) out)`, scope).Eval(scope, nil)
	tt.Equal(t, "AB", out.String())
}

func TestWriteSequenceNilEnd(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString(`(write-sequence "abcdef" out :end nil)`, scope).Eval(scope, nil)

	tt.Equal(t, slip.String("abcdef"), result)
	tt.Equal(t, "abcdef", out.String())
}

func TestWriteSequenceNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(write-sequence "abc" t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWriteSequenceNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source:    `(write-sequence t *standard-output*)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWriteSequenceBadElement(t *testing.T) {
	(&sliptest.Function{
		Source:    `(write-sequence '(t) *standard-output*)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWriteSequenceBadStart(t *testing.T) {
	(&sliptest.Function{
		Source:    `(write-sequence "abc" *standard-output* :start t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(write-sequence "abc" *standard-output* :start 4)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestWriteSequenceBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(write-sequence "abc" *standard-output* :end t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(write-sequence "abc" *standard-output* :start 2 :end 1)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestWriteSequenceWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(write-sequence "abc" out)`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}
