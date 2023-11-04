// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestVersionOk(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("reader", nil)
	_ = slip.ReadString(`(setq reader (parquet-open "testdata/primitive.parquet"))`).Eval(scope, nil)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(parquet-version reader)`,
		Expect: `"v1.0"`,
	}).Test(t)
}

func TestVersionBadReader(t *testing.T) {
	(&sliptest.Function{
		Source:    `(parquet-version 7)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(parquet-version (make-instance 'vanilla-flavor))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
