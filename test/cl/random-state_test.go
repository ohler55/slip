// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestRandomStateNew(t *testing.T) {
	rs := cl.NewRandomState(nil)
	(&sliptest.Object{
		Target:    rs,
		String:    `/#<random-state [0-9]+ [0-9]+>/`,
		Simple:    func(t2 *testing.T, v interface{}) { tt.SameType(t2, "", v) },
		Hierarchy: "random-state.t",
		Equals: []*sliptest.EqTest{
			{Other: rs, Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
		},
		Eval: rs,
	}).Test(t)
}

func TestRandomStateCopy(t *testing.T) {
	current, _ := slip.GetVar("*random-state*")
	rs := cl.NewRandomState(current.(*cl.RandomState))
	tt.Equal(t, true, current.Equal(rs))
}

func TestRandomStateSeed(t *testing.T) {
	rs := cl.NewRandomState(nil)
	rs.Seed(111)
	tt.Equal(t, "#<random-state 111 0>", rs.String())
}
