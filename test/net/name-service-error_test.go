// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNameServiceErrorMake(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-condition 'name-service-error :message "name not found")`,
		Expect: "/#<name-service-error [0-9a-h]+>/",
	}).Test(t)
}
