// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestDribblerObject(t *testing.T) {
	dribbler := &cl.Dribbler{}
	(&sliptest.Object{
		Target:    dribbler,
		String:    "#<DRIBBLER>",
		Simple:    "#<DRIBBLER>",
		Hierarchy: "dribbler.stream.t",
		Equals: []*sliptest.EqTest{
			{Other: dribbler, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			(dribbler).StreamType,
		},
		Eval: dribbler,
	}).Test(t)
	tt.Equal(t, true, dribbler.IsOpen())
}
