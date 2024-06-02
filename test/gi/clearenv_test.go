// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"os"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip/sliptest"
)

func TestClearenv(t *testing.T) {
	orig := os.Environ()
	defer func() {
		for _, ev := range orig {
			parts := strings.Split(ev, "=")
			os.Setenv(parts[0], parts[1])
		}
	}()
	os.Setenv("TEST_VAR", "something")
	(&sliptest.Function{
		Source: `(clearenv)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, "", os.Getenv("TEST_VAR"))
}
