// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEncodeUniversalTimeBasic(t *testing.T) {
	(&sliptest.Function{
		Source: "(encode-universal-time 15 14 13 12 11 2024)",
		Expect: "3940406055",
	}).Test(t)
}

func TestEncodeUniversalTimeZone(t *testing.T) {
	(&sliptest.Function{
		Source: "(encode-universal-time 15 14 13 12 11 2024 5)",
		Expect: "3940424055",
	}).Test(t)
}

func TestEncodeUniversalTimeBadSecond(t *testing.T) {
	(&sliptest.Function{
		Source:    "(encode-universal-time t 14 13 12 11 2024 5)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestEncodeUniversalTimeBadMinute(t *testing.T) {
	(&sliptest.Function{
		Source:    "(encode-universal-time 15 t 13 12 11 2024 5)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestEncodeUniversalTimeBadHour(t *testing.T) {
	(&sliptest.Function{
		Source:    "(encode-universal-time 15 14 t 12 11 2024 5)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestEncodeUniversalTimeBadDate(t *testing.T) {
	(&sliptest.Function{
		Source:    "(encode-universal-time 15 14 13 t 11 2024 5)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestEncodeUniversalTimeBadMonth(t *testing.T) {
	(&sliptest.Function{
		Source:    "(encode-universal-time 15 14 13 12 t 2024 5)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestEncodeUniversalTimeBadYear(t *testing.T) {
	(&sliptest.Function{
		Source:    "(encode-universal-time 15 14 13 12 11 t 5)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestEncodeUniversalTimeBadZone(t *testing.T) {
	(&sliptest.Function{
		Source:    "(encode-universal-time 15 14 13 12 11 2024 t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
