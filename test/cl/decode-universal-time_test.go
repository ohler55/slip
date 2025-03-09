// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDecodeUniversalTimeTime(t *testing.T) {
	(&sliptest.Function{
		Source: "(decode-universal-time @2024-11-12T13:14:15Z)",
		Expect: "/15, 14, [0-9]+, 12, 11, 2024, 1, nil, -{0,1}[0-9]+/",
	}).Test(t)
	(&sliptest.Function{
		Source: "(decode-universal-time @2024-11-12T13:14:15Z 5)",
		Expect: "15, 14, 8, 12, 11, 2024, 1, nil, 5",
	}).Test(t)
}

func TestDecodeUniversalTimeFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: "(decode-universal-time 3940406055)",
		Expect: "/15, 14, [0-9]+, 12, 11, 2024, 1, nil, -{0,1}[0-9]+/",
	}).Test(t)
}

func TestDecodeUniversalTimeDST(t *testing.T) {
	(&sliptest.Function{
		Source: "(decode-universal-time @2024-08-12T13:14:15Z)",
		Expect: "/15, 14, [0-9]+, 12, 8, 2024, 0, t|nil, -{0,1}[0-9]+/",
	}).Test(t)
}

func TestDecodeUniversalTimeRatioZone(t *testing.T) {
	(&sliptest.Function{
		Source: "(decode-universal-time @2024-11-12T13:14:15Z 2.5)",
		Expect: "15, 44, 10, 12, 11, 2024, 1, nil, 5/2",
	}).Test(t)
}

func TestDecodeUniversalTimeBadTime(t *testing.T) {
	(&sliptest.Function{
		Source:    "(decode-universal-time t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDecodeUniversalTimeBadZone(t *testing.T) {
	(&sliptest.Function{
		Source:    "(decode-universal-time 11111111111111 t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
