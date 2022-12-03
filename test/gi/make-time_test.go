// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMakeTimepDay(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-time 2022 07 17)`,
		Expect: "@2022-07-17T00:00:00Z",
	}).Test(t)
}

func TestMakeTimepFull(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-time 2022 july 17 20 53 24 123456789 "EST")`,
		Expect: "@2022-07-17T20:53:24.123456789-05:00",
	}).Test(t)
}

func TestMakeTimepBadArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-time 2022 july 17 20 53 24 123456789 "EST" 1)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(make-time t july 17 20 53 24 123456789 "EST")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(make-time 2022 t 17 20 53 24 123456789 "EST")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(make-time 2022 july t 20 53 24 123456789 "EST")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(make-time 2022 july 17 t 53 24 123456789 "EST")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(make-time 2022 july 17 20 t 24 123456789 "EST")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(make-time 2022 july 17 20 53 t 123456789 "EST")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(make-time 2022 july 17 20 53 24 t "EST")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(make-time 2022 july 17 20 53 24 123456789 t)`,
		Panics: true,
	}).Test(t)
}
