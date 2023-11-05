// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestColumnOk(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("reader", nil)
	_ = slip.ReadString(`(setq reader (parquet-open "testdata/primitive.parquet"))`).Eval(scope, nil)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(parquet-column reader 0)`,
		Expect: "(4 5 6 7 2 3 0 1)",
	}).Test(t)
}

func TestColumnBadReader(t *testing.T) {
	(&sliptest.Function{
		Source:    `(parquet-column 7 0)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(parquet-column (make-instance 'vanilla-flavor) 0)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestColumnBadID(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("reader", nil)
	_ = slip.ReadString(`(setq reader (parquet-open "testdata/primitive.parquet"))`).Eval(scope, nil)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(parquet-column reader t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
