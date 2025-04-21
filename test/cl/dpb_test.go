// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDpbPositivePositive(t *testing.T) {
	(&sliptest.Function{
		Source: "(dpb 1 (byte 1 10) 0)",
		Expect: "1024",
	}).Test(t)
	(&sliptest.Function{
		Source: "(dpb 1 (byte 2 10) 2048)",
		Expect: "1024",
	}).Test(t)
}

func TestDpbNegativePositive(t *testing.T) {
	(&sliptest.Function{
		Source: "(dpb -2 (byte 2 10) 0)",
		Expect: "2048",
	}).Test(t)
}

func TestDpbOverSize(t *testing.T) {
	(&sliptest.Function{
		Source: "(dpb 7 (byte 70 1) 0)",
		Expect: "14",
	}).Test(t)
	(&sliptest.Function{
		Source: "(dpb -7 (byte 70 1) 0)",
		Expect: "2361183241434822606834",
	}).Test(t)
}
