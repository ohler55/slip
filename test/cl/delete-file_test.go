// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"os"
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDeleteFileOk(t *testing.T) {
	_ = os.WriteFile("testdata/remove.bak", []byte("something"), 0666)
	(&sliptest.Function{
		Source: `(delete-file "testdata/*.bak")`,
		Expect: "t",
	}).Test(t)
}

func TestDeleteFileNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete-file t)`,
		Panics: true,
	}).Test(t)
}
