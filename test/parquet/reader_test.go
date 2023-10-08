// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReaderBasic(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/sample.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `reader`,
		Expect: "/^#<parquet-reader-flavor [0-9a-f]+>$/",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :filepath)`,
		Expect: `"testdata/sample.parquet"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :version)`,
		Expect: `"v1.0"`,
	}).Test(t)

	// TBD other methods about the details of the file
}
