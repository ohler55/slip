// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip/sliptest"
)

func TestSetenv(t *testing.T) {
	(&sliptest.Function{
		Source: `(setenv "TEST_VAR" "something")`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "something", os.Getenv("TEST_VAR"))
}

func TestSetenvBadName(t *testing.T) {
	(&sliptest.Function{
		Source: `(setenv t "bad")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(setenv "" "bad")`,
		Panics: true,
	}).Test(t)
}

func TestSetenvBadValue(t *testing.T) {
	(&sliptest.Function{
		Source: `(setenv "BAD" t)`,
		Panics: true,
	}).Test(t)
}
