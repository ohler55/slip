// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNowNoLoc(t *testing.T) {
	(&sliptest.Function{
		Source: `(now)`,
		Expect: "/^@[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}.*Z$/",
	}).Test(t)
}

func TestNowUTC(t *testing.T) {
	(&sliptest.Function{
		Source: `(now 'UTC)`,
		Expect: "/^@[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}.*Z$/",
	}).Test(t)
}

func TestNowLoc(t *testing.T) {
	(&sliptest.Function{
		Source: `(now "EST")`,
		Expect: "/^@[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}.*-05:00$/",
	}).Test(t)
}

func TestNowLocal(t *testing.T) {
	(&sliptest.Function{
		Source: `(now 'local)`,
		Expect: "/^@[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}.*$/",
	}).Test(t)
}

func TestNowBadLoc(t *testing.T) {
	(&sliptest.Function{
		Source: `(now "BAD")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(now 'BAD)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(now t)`,
		Panics: true,
	}).Test(t)
}

func TestNowBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(now 'local t)`,
		Panics: true,
	}).Test(t)
}
