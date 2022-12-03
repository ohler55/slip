// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestOutputStreampTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(output-stream-p *standard-output*)`,
		Expect: "t",
	}).Test(t)
}

func TestOutputStreampFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(output-stream-p t)`,
		Expect: "nil",
	}).Test(t)
}

func TestOutputStreampBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(output-stream-p)`,
		Panics: true,
	}).Test(t)
}
