// Copyright (c) 2023, Peter Ohler, All rights reserved.

package csv_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

type badReader int

func (w badReader) Read([]byte) (int, error) {
	return 0, fmt.Errorf("oops")
}

func TestReadStream(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.InputStream{Reader: strings.NewReader("A,B\n1,2\n3,4\n")}
	scope.Let(slip.Symbol("in"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(csv-read in)`,
		Expect: `(("A" "B") ("1" "2") ("3" "4"))`,
	}).Test(t)
}

func TestReadString(t *testing.T) {
	(&sliptest.Function{
		Source: `(csv-read "A,B\n1,2\n3,4\n")`,
		Expect: `(("A" "B") ("1" "2") ("3" "4"))`,
	}).Test(t)
}

func TestReadSeparator(t *testing.T) {
	(&sliptest.Function{
		Source: `(csv-read "A|B\n1|2\n3|4\n" :separator #\|)`,
		Expect: `(("A" "B") ("1" "2") ("3" "4"))`,
	}).Test(t)
}

func TestReadTrim(t *testing.T) {
	(&sliptest.Function{
		Source: `(csv-read "A, B\n1, 2 \n3, 4\n" :trim t)`,
		Expect: `(("A" "B") ("1" "2") ("3" "4"))`,
	}).Test(t)
}

func TestReadComment(t *testing.T) {
	(&sliptest.Function{
		Source: `(csv-read "A,B\n1,2\n# comment\n3,4\n" :comment-char #\#)`,
		Expect: `(("A" "B") ("1" "2") ("3" "4"))`,
	}).Test(t)
}

func TestReadLazy(t *testing.T) {
	(&sliptest.Function{
		Source: `(csv-read "A,B\n1,2\n3\"x\",4\n" :lazy-quotes t)`,
		Expect: `(("A" "B") ("1" "2") ("3"x"" "4"))`,
	}).Test(t)
}

func TestReadEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(csv-read "A,B\n1,2\n\n3,4\n")`,
		Expect: `(("A" "B") ("1" "2") ("3" "4"))`,
	}).Test(t)
}

func TestReadBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source:    `(csv-read "A,B\n1,2\n3,4\n" :bad t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(csv-read "A,B\n1,2\n3,4\n" t t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestReadStreamError(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.InputStream{Reader: badReader(0)}
	scope.Let(slip.Symbol("in"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(csv-read in)`,
		Panics: true,
	}).Test(t)
}

func TestReadBadInput(t *testing.T) {
	(&sliptest.Function{
		Source:    `(csv-read t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestReadMissingKeywordArg(t *testing.T) {
	(&sliptest.Function{
		Source:    `(csv-read "1,2\n" :trim)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestReadNotChar(t *testing.T) {
	(&sliptest.Function{
		Source:    `(csv-read "1,2\n" :separator t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(csv-read "1,2\n" :comment-char t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
