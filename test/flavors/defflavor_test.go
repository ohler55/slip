// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestDefflavorBasic(t *testing.T) {
	defer undefFlavor("strawberry")

	code := slip.ReadString(`
(defflavor strawberry ((size "medium")) ()
 :gettable-instance-variables
 :settable-instance-variables
 (:initable-instance-variables size)
 (:documentation "Strawberry icecream"))
`)
	scope := slip.NewScope()
	tt.Equal(t, slip.Symbol("strawberry"), code.Eval(scope))

	f := slip.ReadString("strawberry").Eval(scope)
	sf := f.Simplify()
	tt.Equal(t, "strawberry", jp.C("name").First(sf))
	tt.Equal(t, "Strawberry icecream", jp.C("docs").First(sf))

	daemons := jp.MustParseString("methods[*][?(@.name == ':size')]").Get(sf)
	tt.Equal(t, 1, len(daemons))

	daemons = jp.MustParseString("methods[*][?(@.name == ':set-size')]").Get(sf)
	tt.Equal(t, 1, len(daemons))
}

func TestDefflavorGettableSettable(t *testing.T) {
	defer undefFlavor("strawberry")

	code := slip.ReadString(`
(defflavor strawberry ((size "medium") color) ()
 (:gettable-instance-variables size)
 (:settable-instance-variables size))
`)
	scope := slip.NewScope()
	tt.Equal(t, slip.Symbol("strawberry"), code.Eval(scope))

	f := slip.ReadString("strawberry").Eval(scope)
	sf := f.Simplify()

	daemons := jp.MustParseString("methods[*][?(@.name == ':size')]").Get(sf)
	tt.Equal(t, 1, len(daemons))

	daemons = jp.MustParseString("methods[*][?(@.name == ':color')]").Get(sf)
	tt.Equal(t, 0, len(daemons))

	daemons = jp.MustParseString("methods[*][?(@.name == ':set-size')]").Get(sf)
	tt.Equal(t, 1, len(daemons))

	daemons = jp.MustParseString("methods[*][?(@.name == ':set-color')]").Get(sf)
	tt.Equal(t, 0, len(daemons))
}

func TestDefflavorInherit(t *testing.T) {
	defer undefFlavors("f1", "f2", "f3")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f1 ((a 1)) ())
(defflavor f2 ((b 2)) (f1))
(defflavor f3 () (f2))
`).Eval(scope)

	names := slip.ReadString("*all-flavor-names*").Eval(scope)
	tt.Equal(t, "(bag-flavor f1 f2 f3 vanilla-flavor)", names.String())

	sf := slip.ReadString("f3").Eval(scope).Simplify()
	tt.Equal(t, "[f2 f1 vanilla-flavor]", pretty.SEN(jp.C("inherit").First(sf)))
	tt.Equal(t, "{a: 1 b: 2 self: null}", pretty.SEN(jp.C("defaultVars").First(sf)))

	slip.ReadString("(undefflavor 'f1)").Eval(scope)

	// undefflavor should remove all flavors that inherit from f1 as well as f1.
	names = slip.ReadString("*all-flavor-names*").Eval(scope)
	tt.Equal(t, "(bag-flavor vanilla-flavor)", names.String())
}

func TestDefflavorInheritSame(t *testing.T) {
	defer undefFlavors("f1", "f2", "f3")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f1 ((a 1)) ())
(defflavor f2 ((b 2)) (f1))
(defflavor f3 () (f1 f2))
`).Eval(scope)

	sf := slip.ReadString("f3").Eval(scope).Simplify()
	tt.Equal(t, "[f1 f2 vanilla-flavor]", pretty.SEN(jp.C("inherit").First(sf)))
}

func TestDefflavorNoVanilla(t *testing.T) {
	defer undefFlavors("chocolate")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor chocolate ((a 1)) () :no-vanilla-flavor)
`).Eval(scope)

	sf := slip.ReadString("chocolate").Eval(scope).Simplify()
	tt.Equal(t, "[]", pretty.SEN(jp.C("inherit").First(sf)))
}

func TestDefflavorKeywords(t *testing.T) {
	defer undefFlavors("f1")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f1 ((b 2)) () (:init-keywords :x :y) (:required-init-keywords :x))
`).Eval(scope)

	sf := slip.ReadString("f1").Eval(scope).Simplify()
	tt.Equal(t, `[":x"]`, pretty.SEN(jp.C("requiredKeywords").First(sf)))
	tt.Equal(t, `{":x": null ":y": null}`, pretty.SEN(jp.C("keywords").First(sf)))
}

func TestDefflavorInitPlist(t *testing.T) {
	defer undefFlavors("f1")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f1 ((b 2)) () (:default-init-plist (:allow-other-keys t) (:x 1) (:y 2)))
`).Eval(scope)

	sf := slip.ReadString("f1").Eval(scope).Simplify()
	tt.Equal(t, `{":x": 1 ":y": 2}`, pretty.SEN(jp.C("keywords").First(sf)))
}

func TestDefflavorInitPlistInherit(t *testing.T) {
	defer undefFlavors("f1")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f1 ((a 1)) () (:default-init-plist (:allow-other-keys t) (:x 1) (:y 2)))
(defflavor f2 ((b 2)) (f1))
`).Eval(scope)

	sf := slip.ReadString("f2").Eval(scope).Simplify()
	tt.Equal(t, `{":x": 1 ":y": 2}`, pretty.SEN(jp.C("keywords").First(sf)))
}

func TestDefflavorInitBadPlist(t *testing.T) {
	defer undefFlavors("f1")
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f1 ((b 2)) () (:default-init-plist (t 2)))
`).Eval(slip.NewScope())
	})
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f1 ((b 2)) () (:default-init-plist :x))
`).Eval(slip.NewScope())
	})
}

func TestDefflavorBadKeywords(t *testing.T) {
	defer undefFlavors("f1")
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f1 ((b 2)) () (:init-keywords t))
`).Eval(slip.NewScope())
	})
}

func TestDefflavorBadGettable(t *testing.T) {
	defer undefFlavors("f1")
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f1 ((b 2)) () (:gettable-instance-variables t))
`).Eval(slip.NewScope())
	})
}

func TestDefflavorBadSettable(t *testing.T) {
	defer undefFlavors("f1")
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f1 ((b 2)) () (:settable-instance-variables t))
`).Eval(slip.NewScope())
	})
}

func TestDefflavorInclude(t *testing.T) {
	defer undefFlavors("f1", "f2", "f3")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f1 ((a 1)) ())
(defflavor f2 ((b 2)) (f1))
(defflavor f3 () () (:included-flavors f2 f1))
`).Eval(scope)

	names := slip.ReadString("*all-flavor-names*").Eval(scope)
	tt.Equal(t, "(bag-flavor f1 f2 f3 vanilla-flavor)", names.String())

	sf := slip.ReadString("f3").Eval(scope).Simplify()
	tt.Equal(t, "[f2 f1 vanilla-flavor]", pretty.SEN(jp.C("inherit").First(sf)))
	tt.Equal(t, "{a: 1 b: 2 self: null}", pretty.SEN(jp.C("defaultVars").First(sf)))
}

func TestDefflavorIncludeAbstract(t *testing.T) {
	defer undefFlavors("f1", "f2", "f3")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f1 ((a 1)) ())
(defflavor f2 ((b 2)) () :abstract-flavor (:included-flavors f1))
(defflavor f3 () () (:included-flavors f2))
`).Eval(scope)

	names := slip.ReadString("*all-flavor-names*").Eval(scope)
	tt.Equal(t, "(bag-flavor f1 f2 f3 vanilla-flavor)", names.String())

	sf := slip.ReadString("f3").Eval(scope).Simplify()
	tt.Equal(t, "[f2 f1 vanilla-flavor]", pretty.SEN(jp.C("inherit").First(sf)))
	tt.Equal(t, "{a: 1 b: 2 self: null}", pretty.SEN(jp.C("defaultVars").First(sf)))
}

func TestDefflavorIncludeBadAbstract(t *testing.T) {
	defer undefFlavors("f2", "f3")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f2 ((b 2)) () :abstract-flavor (:included-flavors f4))
(defflavor f3 () () (:included-flavors f2))
`).Eval(scope)
	})
	slip.ReadString("(undefflavor 'f2)").Eval(scope)
}

func TestDefflavorBadArgCount(t *testing.T) {
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 ())").Eval(slip.NewScope()) })
}

func TestDefflavorBadName(t *testing.T) {
	tt.Panic(t, func() { slip.ReadString("(defflavor t () ())").Eval(slip.NewScope()) })
}

func TestDefflavorBadDefaultHandler(t *testing.T) {
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () (:default-handler t))").Eval(slip.NewScope()) })
}

func TestDefflavorShortOption(t *testing.T) {
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () (:default-handler))").Eval(slip.NewScope()) })
}

func TestDefflavorTooManyOption(t *testing.T) {
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () (:default-handler bad bad))").Eval(slip.NewScope()) })
}

func TestDefflavorBadOptionKey(t *testing.T) {
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () (t t))").Eval(slip.NewScope()) })
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () :not-an-option)").Eval(slip.NewScope()) })
}

func TestDefflavorBadOption(t *testing.T) {
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () t)").Eval(slip.NewScope()) })
}

func TestDefflavorBadVars(t *testing.T) {
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 t ())").Eval(slip.NewScope()) })
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 (t) ())").Eval(slip.NewScope()) })
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 (x (y)) ())").Eval(slip.NewScope()) })
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 ((1 2)) ())").Eval(slip.NewScope()) })
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 ((x nil) 1) ())").Eval(slip.NewScope()) })
}

func TestDefflavorBadInheritedFlavor(t *testing.T) {
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () (t))").Eval(slip.NewScope()) })
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () t)").Eval(slip.NewScope()) })
}

func TestDefflavorDuplicate(t *testing.T) {
	defer undefFlavor("f1")
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () ()) (defflavor f1 () ())").Eval(slip.NewScope()) })
}

func TestDefflavorInheritNotDefined(t *testing.T) {
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () (bad))").Eval(slip.NewScope()) })
}

func TestDefflavorIncludedNotDefined(t *testing.T) {
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () (:included-flavors bad))").Eval(slip.NewScope()) })
}

func TestDefflavorRequiredNotSymbol(t *testing.T) {
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () (:required-flavors t))").Eval(slip.NewScope()) })
}

func TestDefflavorBadDocs(t *testing.T) {
	tt.Panic(t, func() { slip.ReadString("(defflavor f1 () () (:documentation t))").Eval(slip.NewScope()) })
}

func TestDefflavorBadInitable(t *testing.T) {
	tt.Panic(t,
		func() { slip.ReadString("(defflavor f1 () () (:initable-instance-variables t))").Eval(slip.NewScope()) })
}

func TestDefflavorRequired(t *testing.T) {
	defer undefFlavors("f1", "f2", "f3")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f1 ((a 1)) () :gettable-instance-variables)
(defflavor f2 ((b 2)) (f1))
(defflavor f3 () (f2) (:required-flavors f1) (:required-methods :a) (:required-instance-variables b))
`).Eval(scope)

	names := slip.ReadString("*all-flavor-names*").Eval(scope)
	tt.Equal(t, "(bag-flavor f1 f2 f3 vanilla-flavor)", names.String())

	sf := slip.ReadString("f3").Eval(scope).Simplify()
	tt.Equal(t, "[f2 f1 vanilla-flavor]", pretty.SEN(jp.C("inherit").First(sf)))
}

func TestDefflavorMissing(t *testing.T) {
	defer undefFlavors("f3")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f3 () () (:required-flavors f1))
`).Eval(scope)
	})
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f3 () () (:required-methods :a))
`).Eval(scope)
	})
	tt.Panic(t, func() {
		slip.ReadString(`
(defflavor f3 () () (:required-instance-variables b))
`).Eval(scope)
	})
}
