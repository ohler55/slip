// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip/sliptest"
)

func TestUnsetenv(t *testing.T) {
	os.Setenv("TEST_VAR", "something")
	(&sliptest.Function{
		Source: `(unsetenv "TEST_VAR")`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "", os.Getenv("TEST_VAR"))
}

func TestUnsetenvBadName(t *testing.T) {
	(&sliptest.Function{
		Source: `(unsetenv t)`,
		Panics: true,
	}).Test(t)
}
