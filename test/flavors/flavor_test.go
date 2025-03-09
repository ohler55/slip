// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestFlavorSimple(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor blueberry ((size "medium")) ())
`, scope)
	_ = code.Eval(scope, nil)
	f := slip.ReadString("blueberry", scope).Eval(scope, nil)

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
    [{from: vanilla-flavor name: ":change-class" primary: true}]
    [{from: vanilla-flavor name: ":change-flavor" primary: true}]
    [{from: vanilla-flavor name: ":describe" primary: true}]
    [{from: vanilla-flavor name: ":equal" primary: true}]
    [{from: vanilla-flavor name: ":eval-inside-yourself" primary: true}]
    [{from: vanilla-flavor name: ":flavor" primary: true}]
    [{from: vanilla-flavor name: ":id" primary: true}]
    [{from: vanilla-flavor name: ":init" primary: true}]
    [{from: vanilla-flavor name: ":inspect" primary: true}]
    [{from: vanilla-flavor name: ":operation-handled-p" primary: true}]
    [{from: vanilla-flavor name: ":print-self" primary: true}]
    [{from: vanilla-flavor name: ":send-if-handles" primary: true}]
    [{from: vanilla-flavor name: ":shared-initialize" primary: true}]
    [
      {
        from: vanilla-flavor
        name: ":update-instance-for-different-class"
        primary: true
      }
    ]
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

	f.(*flavors.Flavor).SetDocumentation("quux")
	tt.Equal(t, "quux", f.(*flavors.Flavor).Documentation())
}

func TestFlavorDescribeBasic(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor blueberry ((size "medium")) ())
`, scope)
	_ = code.Eval(scope, nil)
	f := slip.ReadString("blueberry", scope).Eval(scope, nil)

	out := f.(*flavors.Flavor).Describe([]byte{}, 0, 80, false)
	tt.Equal(t, `blueberry is a flavor:
  Inherits: vanilla-flavor
  Variables:
    size = "medium"
  Methods:
    :change-class
    :change-flavor
    :describe
    :equal
    :eval-inside-yourself
    :flavor
    :id
    :init
    :inspect
    :operation-handled-p
    :print-self
    :send-if-handles
    :shared-initialize
    :update-instance-for-different-class
    :which-operations
`, string(out))

	out = f.(*flavors.Flavor).Describe([]byte{}, 0, 80, true)
	tt.Equal(t, "\x1b[1mblueberry\x1b[m is a flavor:\n"+
		"  Inherits: vanilla-flavor\n"+
		"  Variables:\n"+
		"    size = \"medium\"\n"+
		"  Methods:\n"+
		"    :change-class\n"+
		"    :change-flavor\n"+
		"    :describe\n"+
		"    :equal\n"+
		"    :eval-inside-yourself\n"+
		"    :flavor\n"+
		"    :id\n"+
		"    :init\n"+
		"    :inspect\n"+
		"    :operation-handled-p\n"+
		"    :print-self\n"+
		"    :send-if-handles\n"+
		"    :shared-initialize\n"+
		"    :update-instance-for-different-class\n"+
		"    :which-operations\n", string(out))
}

func TestFlavorDescribeOptions(t *testing.T) {
	defer undefFlavors("abbey")
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor abbey ((x 3)) ()
 :abstract-flavor
 :no-vanilla-flavor
 :initable-instance-variables
 (:init-keywords :a :b)
 (:documentation "an abstract")
 (:default-init-plist (:c 3) (:allow-other-keys t))
 (:required-methods :x))
`, scope)
	_ = code.Eval(scope, nil)
	_ = slip.ReadString(`(send (find-flavor 'abbey) :document 'x "x-axis")`, scope).Eval(scope, nil)
	f := slip.ReadString("abbey", scope).Eval(scope, nil)

	out := f.(*flavors.Flavor).Describe([]byte{}, 0, 80, false)
	tt.Equal(t, `abbey is an abstract flavor:
  Documentation:
    an abstract
  Variables:
    x = 3 (initable)
      x-axis
  Keywords with default values:
    :a = nil
    :b = nil
    :c = 3
  Allow Other Keywords: true
  Required Methods:
    :x
`, string(out))

	tt.Equal(t, "abbey", f.(*flavors.Flavor).Name())
	tt.Equal(t, "an abstract", f.(*flavors.Flavor).Documentation())

	tt.Equal(t, f, flavors.Find("Abbey"))
}

func TestFlavorReceive(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor blueberry ((size "medium")) ())
`, scope)
	_ = code.Eval(scope, nil)

	blueberry := scope.Get("blueberry").(slip.Class)
	vanilla := scope.Get("vanilla-flavor").(slip.Class)
	tt.Equal(t, true, blueberry.Inherits(vanilla))
	tt.Equal(t, false, blueberry.Inherits(slip.FindClass("fixnum")))

	result := slip.ReadString("(send blueberry :name)", scope).Eval(scope, nil)
	tt.Equal(t, slip.String("blueberry"), result)

	result = slip.ReadString("(send blueberry :inspect)", scope).Eval(scope, nil)
	tt.SameType(t, &flavors.Instance{}, result)
	tt.Equal(t, `{
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
    [{from: vanilla-flavor name: ":change-class" primary: true}]
    [{from: vanilla-flavor name: ":change-flavor" primary: true}]
    [{from: vanilla-flavor name: ":describe" primary: true}]
    [{from: vanilla-flavor name: ":equal" primary: true}]
    [{from: vanilla-flavor name: ":eval-inside-yourself" primary: true}]
    [{from: vanilla-flavor name: ":flavor" primary: true}]
    [{from: vanilla-flavor name: ":id" primary: true}]
    [{from: vanilla-flavor name: ":init" primary: true}]
    [{from: vanilla-flavor name: ":inspect" primary: true}]
    [{from: vanilla-flavor name: ":operation-handled-p" primary: true}]
    [{from: vanilla-flavor name: ":print-self" primary: true}]
    [{from: vanilla-flavor name: ":send-if-handles" primary: true}]
    [{from: vanilla-flavor name: ":shared-initialize" primary: true}]
    [
      {
        from: vanilla-flavor
        name: ":update-instance-for-different-class"
        primary: true
      }
    ]
    [{from: vanilla-flavor name: ":which-operations" primary: true}]
  ]
  name: blueberry
  required: []
  requiredKeywords: []
  requiredMethods: []
  requiredVars: []
}`, pretty.SEN((result.(*flavors.Instance)).Any))

	result = slip.ReadString("(send blueberry :WHICH-OPERATIONS)", scope).Eval(scope, nil)
	tt.Equal(t, "(:describe :document :inspect :name :which-operations)", slip.ObjectString(result))

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	ansi := slip.CurrentPackage.JustGet("*print-ansi*")
	defer slip.CurrentPackage.Set("*print-ansi*", ansi)
	slip.CurrentPackage.Set("*print-ansi*", nil)

	scope.Let(slip.Symbol("*print-ansi*"), nil)
	_ = slip.ReadString("(send blueberry :describe out)", scope).Eval(scope, nil)
	tt.Equal(t, `blueberry is a flavor:
  Inherits: vanilla-flavor
  Variables:
    size = "medium"
  Methods:
    :change-class
    :change-flavor
    :describe
    :equal
    :eval-inside-yourself
    :flavor
    :id
    :init
    :inspect
    :operation-handled-p
    :print-self
    :send-if-handles
    :shared-initialize
    :update-instance-for-different-class
    :which-operations
`, out.String())

	tt.Panic(t, func() { _ = slip.ReadString("(send blueberry :describe t)", scope).Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString("(send blueberry :not-a-method)", scope).Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString(`(send blueberry :document 'x)`, scope).Eval(scope, nil) })
}
