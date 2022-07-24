// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestInputStreampTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(input-stream-p *standard-input*)`,
		Expect: "t",
	}).Test(t)
}

func TestInputStreampFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(input-stream-p t)`,
		Expect: "nil",
	}).Test(t)
}

func TestInputStreampBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(input-stream-p)`,
		Panics: true,
	}).Test(t)
}
