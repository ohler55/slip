// Copyright (c) 2023, Peter Ohler, All rights reserved.

package csv_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWriteString(t *testing.T) {
	(&sliptest.Function{
		Source: `(csv-write '((a b)(1 2)(3 4)) nil)`,
		Expect: "\"a,b\n1,2\n3,4\n\"",
	}).Test(t)
}

func TestWriteStream(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	stream := slip.OutputStream{Writer: &out}
	scope.Let(slip.Symbol("out"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(csv-write '((a b)(1 2)(3 4)) out)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "a,b\n1,2\n3,4\n", out.String())
}

func TestWriteStdout(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	stream := slip.OutputStream{Writer: &out}
	scope.Let(slip.Symbol("*standard-output*"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(csv-write '((a b)(1 2)(3 4)) t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "a,b\n1,2\n3,4\n", out.String())
}

func TestWriteMixedTypes(t *testing.T) {
	(&sliptest.Function{
		Source: `(csv-write '(("A" "B")(1 nil)(nil 4)) nil)`,
		Expect: "\"A,B\n1,\n,4\n\"",
	}).Test(t)
}

func TestWriteOptions(t *testing.T) {
	(&sliptest.Function{
		Source: `(csv-write '((a b)(1 2)(3 4)) nil :separator #\| :crlf t)`,
		Expect: "\"a|b\r\n1|2\r\n3|4\r\n\"",
	}).Test(t)
}

func TestWriteWriteError(t *testing.T) {
	(&sliptest.Function{
		Source: `(csv-write '((a b)(1 2)(3 4)) :separator #\Return)`,
		Panics: true,
	}).Test(t)
}

func TestWriteNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(csv-write t nil)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(csv-write '(t t) nil)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestWriteBadOutput(t *testing.T) {
	(&sliptest.Function{
		Source:    `(csv-write '((a b)) 7)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestWriteBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source:    `(csv-write '((a b)) nil t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(csv-write '((a b)) :bad)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(csv-write '((a b)) :bad t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(csv-write '((a b)) :separator t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
