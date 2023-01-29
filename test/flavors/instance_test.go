// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestInstance(t *testing.T) {
	defer undefFlavors("blueberry")
	code := slip.ReadString(`
(defflavor blueberry ((size "medium")) ())
(setq berry (make-instance 'blueberry))
`)
	scope := slip.NewScope()
	berry := code.Eval(scope)

	tt.Equal(t, "{flavor: blueberry vars: {size: medium}}", pretty.SEN(berry))
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())

	(&sliptest.Object{
		Target: berry,
		String: "/#<blueberry [0-9a-f]+>/",
		Simple: map[string]any{
			"flavor": "blueberry",
			"vars":   map[string]any{"size": "medium"},
		},
		Hierarchy: "blueberry.instance.t",
		Equals: []*sliptest.EqTest{
			{Other: berry, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: berry,
	}).Test(t)

	out := berry.(*flavors.Instance).Describe([]byte{}, 0, 80, false)
	tt.Equal(t, `/#<blueberry [0-9a-h]+>, an instance of flavor blueberry,
  has instance variable values:
    size: "medium"
/`, string(out))

	out = berry.(*flavors.Instance).Describe([]byte{}, 0, 80, true)
	tt.Equal(t, "/\x1b\\[1m#<blueberry [0-9a-h]+>\x1b\\[m, an instance of flavor \x1b\\[1mblueberry\x1b\\[m,\n"+
		"  has instance variable values:\n    size: \"medium\".*/", string(out))

	b2 := slip.ReadString("(make-instance blueberry)").Eval(slip.NewScope()).(*flavors.Instance)
	tt.Equal(t, true, b2.Equal(berry))

	b2.Set(slip.Symbol("size"), slip.Symbol("large"))
	tt.Equal(t, false, b2.Equal(berry))
}
