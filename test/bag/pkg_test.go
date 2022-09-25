// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPkgTimeFormat(t *testing.T) {
	(&sliptest.Function{
		Source: `(setq *bag-time-format* 'second)`,
		Expect: "second",
	}).Test(t)
	v, _ := slip.GetVar(slip.Symbol("*bag-time-format*"))
	tt.Equal(t, `"second"`, slip.ObjectString(v))

	(&sliptest.Function{
		Source: `(setq *bag-time-format* nil)`,
		Expect: "nil",
	}).Test(t)
	v, _ = slip.GetVar(slip.Symbol("*bag-time-format*"))
	tt.Equal(t, "nil", slip.ObjectString(v))

	(&sliptest.Function{
		Source: `(setq *bag-time-format* t)`,
		Panics: true,
	}).Test(t)
}

func TestPkgTimeWrap(t *testing.T) {
	(&sliptest.Function{
		Source: `(setq *bag-time-wrap* "@")`,
		Expect: `"@"`,
	}).Test(t)
	v, _ := slip.GetVar(slip.Symbol("*bag-time-wrap*"))
	tt.Equal(t, `"@"`, slip.ObjectString(v))

	(&sliptest.Function{
		Source: `(setq *bag-time-wrap* 'time)`,
		Expect: "time",
	}).Test(t)
	v, _ = slip.GetVar(slip.Symbol("*bag-time-wrap*"))
	tt.Equal(t, `"time"`, slip.ObjectString(v))

	(&sliptest.Function{
		Source: `(setq *bag-time-wrap* nil)`,
		Expect: "nil",
	}).Test(t)
	v, _ = slip.GetVar(slip.Symbol("*bag-time-wrap*"))
	tt.Equal(t, "nil", slip.ObjectString(v))

	(&sliptest.Function{
		Source: `(setq *bag-time-wrap* t)`,
		Panics: true,
	}).Test(t)
}
