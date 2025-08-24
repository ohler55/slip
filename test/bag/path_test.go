// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag_test

import (
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/sliptest"
)

func TestBagPath(t *testing.T) {
	(&sliptest.Object{
		Target:    bag.Path(jp.C("a").C("b").C("c")),
		String:    "#<bag-path a.b.c>",
		Simple:    "a.b.c",
		Hierarchy: "bag-path.t",
		Equals: []*sliptest.EqTest{
			{Other: bag.Path(jp.C("a").C("b").C("c")), Expect: true},
			{Other: bag.Path(jp.C("a").C("b").C("x")), Expect: false},
			{Other: slip.True, Expect: false},
		},
		Eval: bag.Path(jp.C("a").C("b").C("c")),
	}).Test(t)
	sliptest.LoadForm(t, bag.Path(jp.C("a").C("b").C("c")))
}
