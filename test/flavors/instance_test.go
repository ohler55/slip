// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"sort"
	"strconv"
	"testing"
	"unsafe"

	"github.com/ohler55/ojg"
	"github.com/ohler55/ojg/oj"
	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestInstanceMisc(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor blueberry ((size "medium")) ())
(setq berry (make-instance 'blueberry))
`, scope)
	berry := code.Eval(scope, nil)

	tt.Equal(t, `/{"flavor":"blueberry","id":"[0-9a-f]+","vars":{"size":"medium"}}/`,
		oj.JSON(berry, &ojg.Options{Sort: true, Indent: 0}))
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())

	(&sliptest.Object{
		Target: berry,
		String: "/#<blueberry [0-9a-f]+>/",
		Simple: map[string]any{
			"flavor": "blueberry",
			"id":     strconv.FormatUint(uint64(uintptr(unsafe.Pointer(berry.(*flavors.Instance)))), 16),
			"vars":   map[string]any{"size": "medium"},
		},
		Hierarchy: "blueberry.vanilla-flavor.instance.t",
		Equals: []*sliptest.EqTest{
			{Other: berry, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: berry,
	}).Test(t)

	out := berry.(*flavors.Instance).Describe([]byte{}, 0, 80, false)
	tt.Equal(t, `/#<blueberry [0-9a-f]+>, an instance of flavor blueberry,
  has instance variable values:
    size: "medium"
/`, string(out))

	out = berry.(*flavors.Instance).Describe([]byte{}, 0, 80, true)
	tt.Equal(t, "/\x1b\\[1m#<blueberry [0-9a-f]+>\x1b\\[m, an instance of flavor \x1b\\[1mblueberry\x1b\\[m,\n"+
		"  has instance variable values:\n    size: \"medium\".*/", string(out))

	b2 := slip.ReadString("(make-instance blueberry)", scope).Eval(scope, nil).(*flavors.Instance)
	tt.Equal(t, true, b2.Equal(berry))
	tt.Equal(t, slip.ReadString(`blueberry`, scope).Eval(scope, nil), b2.Class())

	tt.Equal(t, true, b2.IsA(flavors.Find("blueberry")))
	tt.Equal(t, true, b2.IsA(flavors.Find("vanilla-flavor")))
	_ = slip.ReadString(`(defflavor blackberry () ())`, scope).Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'blackberry)", scope).Eval(scope, nil)
	tt.Equal(t, false, b2.IsA(flavors.Find("blackberry")))

	b2.Set(slip.Symbol("size"), slip.Symbol("large"))
	tt.Equal(t, false, b2.Equal(berry))

	bi := berry.(*flavors.Instance)
	tt.Equal(t, 0, bi.Length())

	bi.Any = []any{1}
	tt.Equal(t, 1, bi.Length())

	bi.Any = map[string]any{"a": 1}
	tt.Equal(t, 1, bi.Length())

	bi.Any = "abc"
	tt.Equal(t, 3, bi.Length())

	_ = slip.ReadString(`(defmethod (blueberry :length) () 5)`, scope).Eval(scope, nil)
	tt.Equal(t, 5, bi.Length())

	_ = slip.ReadString(`(defmethod (blueberry :get-size) () size)`, scope).Eval(scope, nil)
	result := slip.ReadString(`(send berry :get-size)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.String("medium"), result)

	slots := bi.SlotNames()
	sort.Strings(slots)
	tt.Equal(t, "[self size]", pretty.SEN(slots))

	value, has := bi.SlotValue(slip.Symbol("size"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.String("medium"), value)

	bi.SetSlotValue(slip.Symbol("size"), slip.String("large"))
	value, _ = bi.SlotValue(slip.Symbol("size"))
	tt.Equal(t, slip.String("large"), value)
}

func TestInstanceBoundCall(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor blueberry ((size "medium")) () :gettable-instance-variables :settable-instance-variables)
(defmethod (blueberry :length) () 5)
(defmethod (blueberry :double) (x) (* 2 x))
(setq berry (make-instance 'blueberry))
`, scope)
	berry := code.Eval(scope, nil).(*flavors.Instance)

	result := berry.BoundReceive(scope, ":length", nil, 0)
	tt.Equal(t, "5", slip.ObjectString(result))

	bindings := slip.NewScope()
	bindings.Let(slip.Symbol("x"), slip.Fixnum(3))
	result = berry.BoundReceive(scope, ":double", bindings, 0)
	tt.Equal(t, "6", slip.ObjectString(result))

	result = berry.BoundReceive(scope, ":size", nil, 0)
	tt.Equal(t, `"medium"`, slip.ObjectString(result))

	result = berry.BoundReceive(scope, ":set-size", bindings, 0)
	tt.Equal(t, "3", slip.ObjectString(result))

	tt.Panic(t, func() { _ = berry.BoundReceive(scope, ":set-size", slip.NewScope(), 0) })
}

func TestInstanceSimplify(t *testing.T) {
	defer undefFlavors("blueberry")
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor blueberry (me) () :settable-instance-variables)
(setq berry (make-instance 'blueberry))
(send berry :set-me berry)
berry
`, scope)
	berry := code.Eval(scope, nil)

	(&sliptest.Object{
		Target: berry,
		String: "/#<blueberry [0-9a-f]+>/",
		Simple: map[string]any{
			"flavor": "blueberry",
			"id":     strconv.FormatUint(uint64(uintptr(unsafe.Pointer(berry.(*flavors.Instance)))), 16),
			"vars":   map[string]any{"me": berry.String()},
		},
		Hierarchy: "blueberry.vanilla-flavor.instance.t",
		Eval:      berry,
	}).Test(t)
}

func TestInstanceInit(t *testing.T) {
	defer undefFlavors("blueberry", "cranberry")
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor blueberry ((size "medium")) ()
           :gettable-instance-variables
           :inittable-instance-variables
           (:init-keywords :x))
blueberry
`, scope)
	blueberry := code.Eval(scope, nil).(*flavors.Flavor)

	inst := blueberry.MakeInstance()
	inst.Init(scope, slip.List{
		slip.Symbol(":size"), slip.Fixnum(3),
		slip.Symbol(":x"), slip.Fixnum(7),
	}, 0)
	tt.Equal(t, slip.Fixnum(3), inst.(*flavors.Instance).Get(slip.Symbol("size")))

	tt.Panic(t, func() { inst.Init(nil, slip.List{slip.True, slip.Fixnum(3)}, 0) })
	tt.Panic(t, func() { inst.Init(nil, slip.List{slip.Symbol(":self"), slip.Fixnum(3)}, 0) })
	tt.Panic(t, func() { inst.Init(nil, slip.List{slip.Symbol(":nothing"), slip.Fixnum(3)}, 0) })

	code = slip.ReadString(`
(defflavor cranberry ((size "medium")) ()
           :gettable-instance-variables
           :inittable-instance-variables
           (:init-keywords :x)
           (:required-init-keywords :x)
           (:default-init-plist (:allow-other-keys t)))
cranberry
`, scope)
	cranberry := code.Eval(scope, nil).(*flavors.Flavor)
	inst = cranberry.MakeInstance()
	inst.Init(nil, slip.List{
		slip.Symbol(":nothing"), slip.Fixnum(3),
		slip.Symbol(":x"), slip.Fixnum(3),
	}, 0)
	tt.Panic(t, func() { inst.Init(nil, slip.List{}, 0) })
}
