// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestOpenOk(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("reader", nil)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(setq reader (parquet-open "testdata/primitive.parquet"))`,
		Expect: "/#<parquet-reader-flavor [0-9a-f]+>/",
	}).Test(t)
}

func TestOpenBadFile(t *testing.T) {
	(&sliptest.Function{
		Source:    `(parquet-open 7)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
