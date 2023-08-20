// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPackageErrorObj(t *testing.T) {
	cond := slip.NewPackageError(&slip.CLPkg, "not a %s package-error", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<PACKAGE-ERROR [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real package-error", cond.Error())
}

func TestPackageErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Package-Error :package (find-package 'cl))`,
		Expect: "/^#<PACKAGE-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	pe, ok := tf.Result.(slip.PackageError)
	tt.Equal(t, ok, true)
	tt.Equal(t, &slip.CLPkg, pe.Package())
	tt.Equal(t, "", pe.Error())

	tf = sliptest.Function{
		Source: `(make-condition 'Package-Error :package (find-package 'cl) :message "raise")`,
		Expect: "/^#<PACKAGE-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	pe, ok = tf.Result.(slip.PackageError)
	tt.Equal(t, ok, true)
	tt.Equal(t, &slip.CLPkg, pe.Package())
	tt.Equal(t, "raise", pe.Error())
}

func TestPackageErrorPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicPackage(&slip.CLPkg, "raise") })
}
