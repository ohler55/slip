// Copyright (c) 2023, Peter Ohler, All rights reserved.

package csv_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestIterateStream(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.InputStream{Reader: strings.NewReader("A, B\n1, 2\n3, 4\n")}
	scope.Let(slip.Symbol("in"), &stream)
	scope.Set(slip.Symbol("csv-test-out"), slip.List{})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(csv-iterate (lambda (row) (setq csv-test-out (cons row csv-test-out))) in :trim t)`,
		Expect: "nil",
	}).Test(t)
	out := scope.Get(slip.Symbol("csv-test-out"))
	tt.Equal(t, `(("3" "4") ("1" "2") ("A" "B"))`, slip.ObjectString(out))
}

func TestIterateString(t *testing.T) {
	scope := slip.NewScope()
	scope.Set(slip.Symbol("csv-test-out"), slip.List{})
	(&sliptest.Function{
		Scope: scope,
		Source: `(csv-iterate
                   (lambda (row) (setq csv-test-out (cons row csv-test-out)))
                   "A,B\n1,2\n3,4\n")`,
		Expect: "nil",
	}).Test(t)
	out := scope.Get(slip.Symbol("csv-test-out"))
	tt.Equal(t, `(("3" "4") ("1" "2") ("A" "B"))`, slip.ObjectString(out))
}

func TestIterateBag(t *testing.T) {
	scope := slip.NewScope()
	scope.Set(slip.Symbol("csv-test-out"), slip.List{})
	(&sliptest.Function{
		Scope: scope,
		Source: `(csv-iterate
                   (lambda (row) (setq csv-test-out (cons (send row :write :pretty t) csv-test-out)))
                   "A,B\n1,2\n3,4\n" :as-bag t)`,
		Expect: "nil",
	}).Test(t)
	out := scope.Get(slip.Symbol("csv-test-out"))
	tt.Equal(t, `("{A: "3" B: "4"}" "{A: "1" B: "2"}")`, slip.ObjectString(out))
}

func TestIterateBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source:    `(csv-iterate (lambda (row) nil) "A,B\n1,2\n3,4\n" :bad t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestIterateStreamError(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.InputStream{Reader: badReader(0)}
	scope.Let(slip.Symbol("in"), &stream)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(csv-iterate (lambda (row) nil) in)`,
		Panics: true,
	}).Test(t)
}

func TestIterateBadInput(t *testing.T) {
	(&sliptest.Function{
		Source:    `(csv-iterate (lambda (row) nil) t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
