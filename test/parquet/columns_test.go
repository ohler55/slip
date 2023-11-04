// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestColumnsOk(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("reader", nil)
	_ = slip.ReadString(`(setq reader (parquet-open "testdata/primitive.parquet"))`).Eval(scope, nil)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(parquet-columns reader)`,
		Validate: func(t *testing.T, v slip.Object) {
			cols := v.(slip.List)
			tt.Equal(t, 11, len(cols))
			tt.Equal(t, 8, len(cols[0].(slip.List)))
		},
	}).Test(t)
}

func TestColumnsBadReader(t *testing.T) {
	(&sliptest.Function{
		Source:    `(parquet-columns 7)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(parquet-columns (make-instance 'vanilla-flavor))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
