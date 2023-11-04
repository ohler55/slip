// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCloseOk(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("reader", nil)
	_ = slip.ReadString(`(setq reader (parquet-open "testdata/primitive.parquet"))`).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(parquet-close reader)`,
		Expect: "nil",
	}).Test(t)
}

func TestCloseBadReader(t *testing.T) {
	(&sliptest.Function{
		Source:    `(parquet-close 7)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(parquet-close (make-instance 'vanilla-flavor))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
