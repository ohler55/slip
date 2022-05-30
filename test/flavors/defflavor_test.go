// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"fmt"
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/slip"
	"github.com/stretchr/testify/require"
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
	require.Equal(t, slip.Symbol("strawberry"), code.Eval(scope))
	defer undefFlavor("strawberry")

	f := slip.ReadString("strawberry").Eval(scope)
	sf := f.Simplify()
	require.Equal(t, "strawberry", jp.C("name").First(sf))
	require.Equal(t, "Strawberry icecream", jp.C("docs").First(sf))

	daemons := jp.MustParseString("methods[*][?(@.name == ':size')]").Get(sf)
	require.Equal(t, 1, len(daemons))

	daemons = jp.MustParseString("methods[*][?(@.name == ':set-size')]").Get(sf)
	require.Equal(t, 1, len(daemons))
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
	require.Equal(t, "(f1 f2 f3 vanilla-flavor)", names.String())

	sf := slip.ReadString("f3").Eval(scope).Simplify()
	require.Equal(t, "[f2 f1 vanilla-flavor]", pretty.SEN(jp.C("inherit").First(sf)))
	require.Equal(t, "{a: 1 b: 2 self: null}", pretty.SEN(jp.C("defaultVars").First(sf)))

	slip.ReadString("(undefflavor 'f1)").Eval(scope)

	// undefflavor should remove all flavors that inherit from f1 as well as f1.
	names = slip.ReadString("*all-flavor-names*").Eval(scope)
	require.Equal(t, "(vanilla-flavor)", names.String())
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
	require.Equal(t, "(f1 f2 f3 vanilla-flavor)", names.String())

	sf := slip.ReadString("f3").Eval(scope).Simplify()
	require.Equal(t, "[f2 f1 vanilla-flavor]", pretty.SEN(jp.C("inherit").First(sf)))
	require.Equal(t, "{a: 1 b: 2 self: null}", pretty.SEN(jp.C("defaultVars").First(sf)))
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
	require.Equal(t, "(f1 f2 f3 vanilla-flavor)", names.String())

	sf := slip.ReadString("f3").Eval(scope).Simplify()
	require.Equal(t, "[f2 f1 vanilla-flavor]", pretty.SEN(jp.C("inherit").First(sf)))
	require.Equal(t, "{a: 1 b: 2 self: null}", pretty.SEN(jp.C("defaultVars").First(sf)))
}

func TestDefflavorIncludeBadAbstract(t *testing.T) {
	defer undefFlavors("f2", "f3")
	scope := slip.NewScope()
	require.Panics(t, func() {
		slip.ReadString(`
(defflavor f2 ((b 2)) () :abstract-flavor (:included-flavors f4))
(defflavor f3 () () (:included-flavors f2))
`).Eval(scope)
	})
	slip.ReadString("(undefflavor 'f2)").Eval(scope)
}

func TestDefflavorBadArgCount(t *testing.T) {
	require.Panics(t, func() { slip.ReadString("(defflavor f1 ())").Eval(slip.NewScope()) })
}

func TestDefflavorBadName(t *testing.T) {
	require.Panics(t, func() { slip.ReadString("(defflavor t () ())").Eval(slip.NewScope()) })
}

func TestDefflavorBadDefaultHandler(t *testing.T) {
	require.Panics(t, func() { slip.ReadString("(defflavor f1 () () (:default-handler t))").Eval(slip.NewScope()) })
}

func TestDefflavorShortOption(t *testing.T) {
	require.Panics(t, func() { slip.ReadString("(defflavor f1 () () (:default-handler))").Eval(slip.NewScope()) })
}

func TestDefflavorTooManyOption(t *testing.T) {
	require.Panics(t, func() { slip.ReadString("(defflavor f1 () () (:default-handler bad bad))").Eval(slip.NewScope()) })
}

func TestDefflavorBadOptionKey(t *testing.T) {
	require.Panics(t, func() { slip.ReadString("(defflavor f1 () () (t t))").Eval(slip.NewScope()) })
}

func TestDefflavorBadOption(t *testing.T) {
	require.Panics(t, func() { slip.ReadString("(defflavor f1 () () t)").Eval(slip.NewScope()) })
}

func TestDefflavorBadVars(t *testing.T) {
	require.Panics(t, func() { slip.ReadString("(defflavor f1 t ())").Eval(slip.NewScope()) })
	require.Panics(t, func() { slip.ReadString("(defflavor f1 (t) ())").Eval(slip.NewScope()) })
	require.Panics(t, func() { slip.ReadString("(defflavor f1 (x (y)) ())").Eval(slip.NewScope()) })
	require.Panics(t, func() { slip.ReadString("(defflavor f1 ((1 2)) ())").Eval(slip.NewScope()) })
	require.Panics(t, func() { slip.ReadString("(defflavor f1 ((x nil) 1) ())").Eval(slip.NewScope()) })
}

func TestDefflavorBadInheritedFlavor(t *testing.T) {
	require.Panics(t, func() { slip.ReadString("(defflavor f1 () (t))").Eval(slip.NewScope()) })
	require.Panics(t, func() { slip.ReadString("(defflavor f1 () t)").Eval(slip.NewScope()) })
}

func TestDefflavorDuplicate(t *testing.T) {
	defer undefFlavor("f1")
	require.Panics(t, func() { slip.ReadString("(defflavor f1 () ()) (defflavor f1 () ())").Eval(slip.NewScope()) })
}

func TestDefflavorInheritNotDefined(t *testing.T) {
	require.Panics(t, func() { slip.ReadString("(defflavor f1 () (bad))").Eval(slip.NewScope()) })
}

func TestDefflavorIncludedNotDefined(t *testing.T) {
	require.Panics(t, func() { slip.ReadString("(defflavor f1 () () (:included-flavors bad))").Eval(slip.NewScope()) })
}

func TestDefflavorRequiredNotSymbol(t *testing.T) {
	require.Panics(t, func() { slip.ReadString("(defflavor f1 () () (:required-flavors t))").Eval(slip.NewScope()) })
}

func TestDefflavorBadDocs(t *testing.T) {
	require.Panics(t, func() { slip.ReadString("(defflavor f1 () () (:documentation t))").Eval(slip.NewScope()) })
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
	require.Equal(t, "(f1 f2 f3 vanilla-flavor)", names.String())

	sf := slip.ReadString("f3").Eval(scope).Simplify()
	require.Equal(t, "[f2 f1 vanilla-flavor]", pretty.SEN(jp.C("inherit").First(sf)))
}

func TestDefflavorMissing(t *testing.T) {
	defer undefFlavors("f3")
	scope := slip.NewScope()
	require.Panics(t, func() {
		slip.ReadString(`
(defflavor f3 () () (:required-flavors f1))
`).Eval(scope)
	})
	require.Panics(t, func() {
		slip.ReadString(`
(defflavor f3 () () (:required-methods :a))
`).Eval(scope)
	})
	require.Panics(t, func() {
		slip.ReadString(`
(defflavor f3 () () (:required-instance-variables b))
`).Eval(scope)
	})
}

func undefFlavors(fns ...string) {
	for _, fn := range fns {
		undefFlavor(fn)
	}
}

func undefFlavor(fn string) {
	defer func() { recover() }()
	slip.ReadString(fmt.Sprintf("(undefflavor '%s)", fn)).Eval(slip.NewScope())
}
