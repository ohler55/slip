// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"testing"

	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestFlavorSimple(t *testing.T) {
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

func TestFlavorDescribeBasic(t *testing.T) {
	defer undefFlavors("blueberry")
	code := slip.ReadString(`
(defflavor blueberry ((size "medium")) ())
`)
	scope := slip.NewScope()
	_ = code.Eval(scope)
	f := slip.ReadString("blueberry").Eval(scope)

	out := f.(*flavors.Flavor).Describe([]byte{}, 0, 80, false)
	tt.Equal(t, `blueberry is a flavor:
  Inherits: vanilla-flavor
  Variables:
    size = "medium"
  Methods:
    :describe
    :eval-inside-yourself
    :id
    :init
    :operation-handler-p
    :print-self
    :send-if-handles
    :which-operations
`, string(out))

	out = f.(*flavors.Flavor).Describe([]byte{}, 0, 80, true)
	tt.Equal(t, "\x1b[1mblueberry\x1b[m is a flavor:\n"+
		"  Inherits: vanilla-flavor\n"+
		"  Variables:\n"+
		"    size = \"medium\"\n"+
		"  Methods:\n"+
		"    :describe\n"+
		"    :eval-inside-yourself\n"+
		"    :id\n"+
		"    :init\n"+
		"    :operation-handler-p\n"+
		"    :print-self\n"+
		"    :send-if-handles\n"+
		"    :which-operations\n", string(out))

}

func TestFlavorDescribeOptions(t *testing.T) {
	defer undefFlavors("abbey")
	code := slip.ReadString(`
(defflavor abbey ((x 3)) ()
 :abstract-flavor
 :no-vanilla-flavor
 :initable-instance-variables
 (:init-keywords :a :b)
 (:documentation "an abstract")
 (:default-init-plist (:c 3) (:allow-other-keys t))
 (:required-methods :x))
`)
	scope := slip.NewScope()
	_ = code.Eval(scope)
	f := slip.ReadString("abbey").Eval(scope)

	out := f.(*flavors.Flavor).Describe([]byte{}, 0, 80, false)
	tt.Equal(t, `abbey is an abstract flavor:
  Documentation:
    an abstract
  Variables:
    x = 3 (initable)
  Keywords with default values:
    :a = nil
    :b = nil
    :c = 3
  Allow Other Keywords: true
  Required Methods:
    :x
`, string(out))
}
