// Copyright (c) 2026, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRef(t *testing.T) {
	defer undefFlavor("reef")
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor reef ((size 3) (none nil)) ())
(setq rr (make-instance 'reef))
`, scope)
	rr := code.Eval(scope, nil).(slip.Instance)
	ref := slip.Ref{Instance: rr, Key: slip.Symbol("size")}
	(&sliptest.Object{
		Target:    &ref,
		String:    "3",
		Simple:    int64(3),
		Hierarchy: "fixnum.integer.rational.real.number.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Fixnum(3), Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: &ref,
	}).Test(t)

	ref.Key = slip.Symbol("none")
	tt.Equal(t, "nil", string(ref.Append(nil)))

	ref.Key = slip.Symbol("quux")
	tt.Panic(t, func() { _ = ref.Get() })
}
