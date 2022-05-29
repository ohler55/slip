// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/slip"
	"github.com/stretchr/testify/require"
)

func TestDefflavorBasic(t *testing.T) {
	code := slip.ReadString(`
(defflavor strawberry ((size "medium")) ()
 :gettable-instance-variables
 :settable-instance-variables
 (:initable-instance-variables size)
 (:documentation "Strawberry icecream"))
`)
	scope := slip.NewScope()
	require.Equal(t, slip.Symbol("strawberry"), code.Eval(scope))
	defer slip.ReadString("(undefflavor 'strawberry)").Eval(scope)

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

	slip.ReadString("(undefflavor 'f1)").Eval(scope)

	// undefflavor should remove all flavors that inherit from f1 as well as f1.
	names = slip.ReadString("*all-flavor-names*").Eval(scope)
	require.Equal(t, "(vanilla-flavor)", names.String())
}

func TestDefflavorBadArgCount(t *testing.T) {
	require.Panics(t, func() { slip.ReadString("(defflavor f1 ())").Eval(slip.NewScope()) })
}

func TestDefflavorBadName(t *testing.T) {
	require.Panics(t, func() { slip.ReadString("(defflavor t () ())").Eval(slip.NewScope()) })
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
	require.Panics(t, func() { slip.ReadString("(defflavor f1 () ()) (defflavor f1 () ())").Eval(slip.NewScope()) })
	slip.ReadString("(undefflavor 'f1)").Eval(slip.NewScope())
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
