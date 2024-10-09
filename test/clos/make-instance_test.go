// Copyright (c) 2022, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/ojg"
	"github.com/ohler55/ojg/oj"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/clos"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeInstanceSimple(t *testing.T) {
	defer undefFlavors("blueberry")
	code := slip.ReadString(`
(defflavor blueberry ((size "medium")) () :initable-instance-variables)
(setq bb (make-instance 'blueberry :size 'small))
`)
	scope := slip.NewScope()
	berry := code.Eval(scope, nil)

	tt.Equal(t, `/{"flavor":"blueberry","id":"[0-9a-f]+","vars":{"size":"small"}}/`,
		oj.JSON(berry, &ojg.Options{Sort: true, Indent: 0}))
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestMakeInstanceKeywords(t *testing.T) {
	defer undefFlavors("blueberry")
	code := slip.ReadString(`
(defflavor blueberry () () (:default-init-plist (:x 3)))
(setq bb (make-instance 'blueberry :x 4))
`)
	scope := slip.NewScope()
	berry := code.Eval(scope, nil)

	tt.Equal(t, `/{"flavor":"blueberry","id":"[0-9a-f]+","vars":{}}/`,
		oj.JSON(berry, &ojg.Options{Sort: true, Indent: 0}))
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestMakeInstanceOtherKeywords(t *testing.T) {
	defer undefFlavors("blueberry")
	code := slip.ReadString(`
(defflavor blueberry () () (:default-init-plist (:allow-other-keys t)))
(setq bb (make-instance 'blueberry :x 4))
`)
	scope := slip.NewScope()
	berry := code.Eval(scope, nil)
	tt.Equal(t, "/#<blueberry [0-9a-f]+>/", berry.String())
}

func TestMakeInstanceBadArgCount(t *testing.T) {
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance)`).Eval(slip.NewScope(), nil) })
}

func TestMakeInstanceNotFlavor(t *testing.T) {
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance t)`).Eval(slip.NewScope(), nil) })
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance 'not-a-flavor)`).Eval(slip.NewScope(), nil) })
}

func TestMakeInstanceAbstract(t *testing.T) {
	defer undefFlavors("blueberry")
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor blueberry ((size "medium")) () :abstract-flavor)
(make-instance blueberry)
`).Eval(slip.NewScope(), nil)
	})
}

func TestMakeInstanceBadKeyword(t *testing.T) {
	defer undefFlavors("blueberry", "raspberry", "blackberry", "cherry")
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor blueberry () ())
(make-instance 'blueberry t nil)
`).Eval(slip.NewScope(), nil)
	})
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor raspberry () ())
(make-instance raspberry bad nil)
`).Eval(slip.NewScope(), nil)
	})
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor blackberry () ())
(make-instance blackberry :self nil)
`).Eval(slip.NewScope(), nil)
	})
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor cherry () ())
(make-instance cherry :x nil)
`).Eval(slip.NewScope(), nil)
	})
}

func TestMakeInstanceMissingKeyword(t *testing.T) {
	defer undefFlavors("blueberry")
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor blueberry () () (:required-init-keywords :x))
(make-instance 'blueberry)
`).Eval(slip.NewScope(), nil)
	})
}

func TestMakeInstanceNotFound(t *testing.T) {
	tt.Panic(t, func() {
		_ = slip.ReadString(`(make-instance 'nothing)`).Eval(slip.NewScope(), nil)
	})
}

func TestMakeInstanceBuiltIn(t *testing.T) {
	tt.Panic(t, func() {
		_ = slip.ReadString(`(make-instance 'fixnum)`).Eval(slip.NewScope(), nil)
	})
	tt.Panic(t, func() {
		_ = slip.ReadString(`(make-instance (find-class 'fixnum))`).Eval(slip.NewScope(), nil)
	})
}

type alphaXCaller struct{}

func (caller alphaXCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	return s.Get("x")
}

func (caller alphaXCaller) Docs() string {
	return `__:x__

Returns the value of x.
`
}

func TestMakeInstanceClass(t *testing.T) {
	a := clos.DefClass("alpha", "an alpha", map[string]slip.Object{"x": slip.Fixnum(7)}, nil, false)
	a.DefMethod(":x", "", alphaXCaller{})

	b := clos.DefClass("bravo", "a bravo", map[string]slip.Object{"y": nil}, []*clos.Class{a}, false)
	c := clos.DefClass(
		"charlie",
		"a charlie",
		nil,
		[]*clos.Class{b, a, slip.FindClass("standard-object").(*clos.Class)},
		false,
	)
	des := string(c.Describe(nil, 2, 100, false))
	tt.Equal(t, "/:x/", des)
	tt.Equal(t, "/Class precedence list: bravo alpha standard-object/", des)

	(&sliptest.Function{
		Source: `(send (make-instance 'charlie) :flavor)`,
		Expect: "#<class charlie>",
	}).Test(t)
	(&sliptest.Function{
		Source: `(send (make-instance 'charlie :x 5) :x)`,
		Expect: "5",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-instance 'charlie t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-instance 'charlie :self t)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-instance 'charlie :self t)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-instance 'charlie :z 1)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
