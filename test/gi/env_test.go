// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"os"
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestEnv(t *testing.T) {
	os.Clearenv()
	os.Setenv("TEST_VAR", "something")
	(&sliptest.Function{
		Source: `(env)`,
		Expect: `(("TEST_VAR" . "something"))`,
	}).Test(t)
}
