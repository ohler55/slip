// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRowsOk(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("reader", nil)
	_ = slip.ReadString(`(setq reader (parquet-open "testdata/primitive.parquet"))`).Eval(scope, nil)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(parquet-rows reader :assoc '(0 2))`,
		Validate: func(t *testing.T, v slip.Object) {
			vlist := v.(slip.List)
			tt.Equal(t, 8, len(vlist))
			tt.Equal(t, `(("id" . 4) ("tinyint_col" . 0))`, vlist[0].String())
		},
	}).Test(t)
}

func TestRowsBadReader(t *testing.T) {
	(&sliptest.Function{
		Source:    `(parquet-rows 7 :assoc)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(parquet-rows (make-instance 'vanilla-flavor) :list)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestRowsBadFormat(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("reader", nil)
	_ = slip.ReadString(`(setq reader (parquet-open "testdata/primitive.parquet"))`).Eval(scope, nil)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(parquet-rows reader :bad)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
