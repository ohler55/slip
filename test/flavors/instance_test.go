// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"

	"github.com/stretchr/testify/require"
)

func TestInstance(t *testing.T) {
	code := slip.ReadString(`
(defflavor blueberry ((size "medium")) ())
(setq berry (make-instance 'blueberry))
`)
	scope := slip.NewScope()
	berry := code.Eval(scope)
	defer slip.ReadString("(undefflavor 'blueberry)").Eval(scope)

	bi := berry.(*flavors.Instance)
	bi.Pocket = 7
	require.Equal(t, "{flavor: blueberry pocket: 7 vars: {size: medium}}", pretty.SEN(berry))
	require.Regexp(t, "#<blueberry [0-9a-f]+>", berry.String())

	(&sliptest.Object{
		Target: berry,
		String: "/#<blueberry [0-9a-f]+>/",
		Simple: map[string]any{
			"flavor": "blueberry",
			"pocket": int64(7),
			"vars":   map[string]any{"size": "medium"},
		},
		Hierarchy: "instance.t",
		Equals: []*sliptest.EqTest{
			{Other: berry, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: berry,
	}).Test(t)
}
