// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestTimeUtcOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-utc @2022-07-10T12:29:21-05:00)`,
		Expect: "@2022-07-10T17:29:21Z",
	}).Test(t)
}

func TestTimeUtcBadArgsCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-utc @2022-07-10T17:29:21Z t)`,
		Panics: true,
	}).Test(t)
}

func TestTimeUtcNotTime(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-utc 7)`,
		Panics: true,
	}).Test(t)
}
