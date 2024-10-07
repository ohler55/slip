// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNameServiceError(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-instance 'name-service-error)`,
		Expect: "/#<name-service-error [0-9a-h]+>/",
	}).Test(t)
}
