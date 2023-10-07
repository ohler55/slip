// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestPkgVars(t *testing.T) {
	(&sliptest.Function{
		Source: `*parquet*`,
		Expect: "#<package parquet>",
	}).Test(t)
}
