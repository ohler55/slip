// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestTimeInOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-in @2022-07-10T17:29:21Z "EST")`,
		Expect: "@2022-07-10T12:29:21-05:00",
	}).Test(t)
}

func TestTimeInBadArgsCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-in @2022-07-10T17:29:21Z)`,
		Panics: true,
	}).Test(t)
}

func TestTimeInNotTime(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-in 7 "EST")`,
		Panics: true,
	}).Test(t)
}
