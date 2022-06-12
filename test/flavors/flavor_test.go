// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"testing"

	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFlavor(t *testing.T) {
	defer undefFlavors("blueberry")
	code := slip.ReadString(`
(defflavor blueberry ((size "medium")) ())
`)
	scope := slip.NewScope()
	_ = code.Eval(scope)
	f := slip.ReadString("blueberry").Eval(scope)

	simple := sen.MustParse([]byte(`{
  abstract: false
  allowOtherKeys: false
  defaultHandler: false
  defaultVars: {self: null size: medium}
  docs: ""
  included: []
  inherit: [vanilla-flavor]
  initable: {}
  keywords: {}
  methods: [
    [{from: vanilla-flavor name: ":describe" primary: true}]
    [{from: vanilla-flavor name: ":eval-inside-yourself" primary: true}]
    [{from: vanilla-flavor name: ":id" primary: true}]
    [{from: vanilla-flavor name: ":init" primary: true}]
    [{from: vanilla-flavor name: ":operation-handler-p" primary: true}]
    [{from: vanilla-flavor name: ":print-self" primary: true}]
    [{from: vanilla-flavor name: ":send-if-handles" primary: true}]
    [{from: vanilla-flavor name: ":which-operations" primary: true}]
  ]
  name: blueberry
  required: []
  requiredKeywords: []
  requiredMethods: []
  requiredVars: []
}`))

	(&sliptest.Object{
		Target:    f,
		String:    "#<flavor blueberry>",
		Simple:    simple,
		Hierarchy: "flavor.t",
		Equals: []*sliptest.EqTest{
			{Other: f, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: f,
	}).Test(t)
}
