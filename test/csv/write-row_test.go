// Copyright (c) 2023, Peter Ohler, All rights reserved.

package csv_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWriteRowString(t *testing.T) {
	(&sliptest.Function{
		Source: `(csv-write-row '(a b) nil)`,
		Expect: "\"a,b\n\"",
	}).Test(t)
}

func TestWriteRowStream(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	stream := slip.OutputStream{Writer: &out}
	scope.Let(slip.Symbol("out"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(csv-write-row '(a b) out)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "a,b\n", out.String())
}

func TestWriteRowStdout(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	stream := slip.OutputStream{Writer: &out}
	scope.Let(slip.Symbol("*standard-output*"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(csv-write-row '(a b) t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "a,b\n", out.String())
}

func TestWriteRowMixedTypes(t *testing.T) {
	(&sliptest.Function{
		Source: `(csv-write-row '("A" nil) nil)`,
		Expect: "\"A,\n\"",
	}).Test(t)
}

func TestWriteRowOptions(t *testing.T) {
	(&sliptest.Function{
		Source: `(csv-write-row '(a b) nil :separator #\| :crlf t)`,
		Expect: "\"a|b\r\n\"",
	}).Test(t)
}

func TestWriteRowWriteRowError(t *testing.T) {
	(&sliptest.Function{
		Source: `(csv-write-row '(a b) :separator #\Return)`,
		Panics: true,
	}).Test(t)
}

func TestWriteRowNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(csv-write-row t nil)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(csv-write-row t nil)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestWriteRowBadOutput(t *testing.T) {
	(&sliptest.Function{
		Source:    `(csv-write-row '(a b) 7)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestWriteRowBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source:    `(csv-write-row '(a b) nil t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(csv-write-row '(a b) :bad)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(csv-write-row '(a b) :bad t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(csv-write-row '(a b) :separator t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
