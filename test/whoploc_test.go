// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWhopLoc(t *testing.T) {
	wl := slip.WhopLoc{
		Combinations: []*slip.Combination{},
		Current:      2,
	}
	(&sliptest.Object{
		Target:    &wl,
		String:    `#<whopper-location 2>`,
		Simple:    "#<whopper-location 2>",
		Hierarchy: "whopper-location.t",
		Equals: []*sliptest.EqTest{
			{Other: &wl, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: &wl,
	}).Test(t)
}
