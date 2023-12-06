// Copyright (c) 2023, Peter Ohler, All rights reserved.

package xml_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWriteString(t *testing.T) {
	(&sliptest.Function{
		Source: `(xml-write '(top ((id . abc)) (child nil "some text")) nil)`,
		Expect: `"<top id="abc"><child>some text</child></top>"`,
	}).Test(t)
}

func TestWriteIndent(t *testing.T) {
	(&sliptest.Function{
		Source: `(xml-write '(top ((id . abc)) (child nil "some text")) nil :indent "  ")`,
		Expect: `"<top id="abc">
  <child>some text</child>
</top>"`,
	}).Test(t)
}

func TestWriteDirective(t *testing.T) {
	(&sliptest.Function{
		Source: `(xml-write '((:directive "DOCTYPE sample") (top ((id . abc)))) nil)`,
		Expect: `"<!DOCTYPE sample><top id="abc"></top>"`,
	}).Test(t)
}

func TestWriteProcInst(t *testing.T) {
	(&sliptest.Function{
		Source: `(xml-write '((:processing-instruction "xml" "version=\"1.0\"") (top ((id . abc)))) nil)`,
		Expect: `"<?xml version="1.0"?><top id="abc"></top>"`,
	}).Test(t)
}

func TestWriteComment(t *testing.T) {
	(&sliptest.Function{
		Source: `(xml-write '((:comment "a comment") (top ((id . abc)))) nil)`,
		Expect: `"<!--a comment--><top id="abc"></top>"`,
	}).Test(t)
}

func TestWriteStream(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	stream := slip.OutputStream{Writer: &out}
	scope.Let(slip.Symbol("out"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(xml-write '("top" (("id" . "abc")) (child nil "some text")) out)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, `<top id="abc"><child>some text</child></top>`, out.String())
}

func TestWriteStdout(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	stream := slip.OutputStream{Writer: &out}
	scope.Let(slip.Symbol("*standard-output*"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(xml-write '(top ((id . abc)) (child nil "some text")) t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, `<top id="abc"><child>some text</child></top>`, out.String())
}

func TestWriteBadData(t *testing.T) {
	(&sliptest.Function{
		Source:    `(xml-write t nil)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(xml-write '(top) nil)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(xml-write '(:comment t) nil)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(xml-write '(:directive t) nil)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(xml-write '(:processing-instruction t) nil)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(xml-write '(:processing-instruction "xx" t) nil)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(xml-write '(top t) nil)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(xml-write '(t ()) nil)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(xml-write '(top (t)) nil)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(xml-write '(top ((7 7))) nil)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(xml-write '(top ((x . 7))) nil)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(xml-write '(top () 7) nil)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestWriteBadOutput(t *testing.T) {
	(&sliptest.Function{
		Source:    `(xml-write '(top () "text") 7)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestWriteBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source:    `(xml-write '(top () "text") nil t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(xml-write '(top () "text") nil :bad t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(xml-write '(top () "text") nil :indent)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(xml-write '(top () "text") nil :indent t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

type badWriter int

func (w badWriter) Write([]byte) (int, error) {
	return 0, fmt.Errorf("oops")
}

func TestWriteOutputError(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	// A small XML with a bad writer will fail on flush.
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(xml-write '(top () "text") out)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	// bufio.Writer flushes at 4096 so a long string is used tp have it fail.
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(xml-write (list 'top '() (list :comment (string-repeat "x" 4096))) out)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(xml-write (list 'top '() (list :directive (string-repeat "x" 4096))) out)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(xml-write (list 'top '() (list :processing-instruction (string-repeat "x" 4096) "x")) out)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(xml-write (list (string-repeat "x" 4096) ()) out)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(xml-write (list (string-repeat "x" 2048) () "x") out)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(xml-write (list 'top '() (string-repeat "x" 4096)) out)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}
