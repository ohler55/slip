// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestPackageErrorObj(t *testing.T) {
	cond := slip.NewPackageError(&slip.CLPkg, "not a %s package-error", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<package-error [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) {
			_, ok := v.(map[string]any)
			tt.Equal(t2, true, ok)
		},
		Eval: cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real package-error", cl.SimpleCondMsg(slip.NewScope(), cond.(slip.Instance)))
}

func TestPackageErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Package-Error :package (find-package 'cl))`,
		Expect: "/^#<package-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	pe, ok := tf.Result.(slip.Instance)
	tt.Equal(t, ok, true)
	value, has := pe.SlotValue(slip.Symbol("package"))
	tt.Equal(t, has, true)
	tt.Equal(t, &slip.CLPkg, value)

	tf = sliptest.Function{
		Source: `(make-condition 'Package-Error :package (find-package 'cl) :message "raise")`,
		Expect: "/^#<package-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	pe, ok = tf.Result.(slip.Instance)
	tt.Equal(t, ok, true)
	value, has = pe.SlotValue(slip.Symbol("message"))
	tt.Equal(t, has, true)
	tt.Equal(t, slip.String("raise"), value)
}

func TestPackageErrorPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicPackage(&slip.CLPkg, "raise") })
}
