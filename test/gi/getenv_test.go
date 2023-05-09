// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"os"
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestGetenv(t *testing.T) {
	os.Setenv("TEST_VAR", "something")
	(&sliptest.Function{
		Source: `(getenv "TEST_VAR")`,
		Expect: `"something"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(getenv "TEST_NOT_SET_VAR")`,
		Expect: "nil",
	}).Test(t)
}

func TestGetenvBadName(t *testing.T) {
	(&sliptest.Function{
		Source: `(getenv t)`,
		Panics: true,
	}).Test(t)
}
