// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestTimeFormatOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-format @2022-07-10T17:29:21Z "2006-01-02")`,
		Expect: `"2022-07-10"`,
	}).Test(t)
}

func TestTimeFormatBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-format @2022-07-10T17:29:21Z)`,
		Panics: true,
	}).Test(t)
}

func TestTimeFormatNotTime(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-format t "2006-01-02")`,
		Panics: true,
	}).Test(t)
}

func TestTimeFormatBadLayout(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-format @2022-07-10T17:29:21Z t)`,
		Panics: true,
	}).Test(t)
}
