// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"os"
	"strings"
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestEnv(t *testing.T) {
	orig := os.Environ()
	defer func() {
		for _, ev := range orig {
			parts := strings.Split(ev, "=")
			_ = os.Setenv(parts[0], parts[1])
		}
	}()
	os.Clearenv()
	_ = os.Setenv("TEST_VAR", "something")
	(&sliptest.Function{
		Source: `(env)`,
		Expect: `(("TEST_VAR" . "something"))`,
	}).Test(t)
}
