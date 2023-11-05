// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEachRowOk(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("reader", nil)
	_ = slip.ReadString(`(setq reader (parquet-open "testdata/primitive.parquet") rows nil)`).Eval(scope, nil)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(parquet-each-row reader (lambda (row) (setq rows (add rows row))) :assoc '(0 2))`,
		Expect: "nil",
	}).Test(t)
	rows := scope.Get("rows").(slip.List)
	tt.Equal(t, 8, len(rows))
	tt.Equal(t, `(("id" . 4) ("tinyint_col" . 0))`, slip.ObjectString(rows[0]))
}

func TestEachRowBadReader(t *testing.T) {
	(&sliptest.Function{
		Source:    `(parquet-each-row 7 (lambda (row) nil) :assoc)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(parquet-each-row (make-instance 'vanilla-flavor) (lambda (row) nil) :list)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestEachRowBadFormat(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("reader", nil)
	_ = slip.ReadString(`(setq reader (parquet-open "testdata/primitive.parquet"))`).Eval(scope, nil)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(parquet-each-row reader (lambda (row) nil) :bad)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
