// Copyright (c) 2023, Peter Ohler, All rights reserved.

package csv_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/pkg/gi"
	"github.com/ohler55/slip/sliptest"
)

func TestIterateStream(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.NewInputStream(strings.NewReader("A, B\n1, 2\n3, 4\n"))
	scope.Let(slip.Symbol("in"), stream)
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

func TestIterateChannel(t *testing.T) {
	scope := slip.NewScope()
	channel := make(gi.Channel, 10)
	scope.Let(slip.Symbol("chan"), channel)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(csv-iterate chan "A,B\n1,2\n3,4\n")`,
		Expect: "nil",
	}).Test(t)
	var result slip.List
	for i := 0; i < 3; i++ {
		result = append(result, <-channel)
	}
	tt.Equal(t, `(("A" "B") ("1" "2") ("3" "4"))`, result.String())

	(&sliptest.Function{
		Scope:  scope,
		Source: `(csv-iterate chan "A,B\n1,2\n3,4\n" :as-bag t)`,
		Expect: "nil",
	}).Test(t)
	result = result[:0]
	for i := 0; i < 2; i++ {
		b := <-channel
		b = b.(*flavors.Instance).Receive(scope, ":native", nil, 0)
		result = append(result, b)
	}
	scope.Let("result", result)
	tt.Equal(t, `"1"`, slip.ObjectString(slip.ReadString(`(cdr (assoc "A" (car result)))`, scope).Eval(scope, nil)))
	tt.Equal(t, `"2"`, slip.ObjectString(slip.ReadString(`(cdr (assoc "B" (car result)))`, scope).Eval(scope, nil)))
	tt.Equal(t, `"3"`, slip.ObjectString(slip.ReadString(`(cdr (assoc "A" (cadr result)))`, scope).Eval(scope, nil)))
	tt.Equal(t, `"4"`, slip.ObjectString(slip.ReadString(`(cdr (assoc "B" (cadr result)))`, scope).Eval(scope, nil)))
}

func TestIterateBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source:    `(csv-iterate (lambda (row) nil) "A,B\n1,2\n3,4\n" :bad t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestIterateStreamError(t *testing.T) {
	scope := slip.NewScope()
	stream := slip.NewInputStream(badReader(0))
	scope.Let(slip.Symbol("in"), stream)
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
