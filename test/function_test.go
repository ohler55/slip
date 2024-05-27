// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFunctionNew(t *testing.T) {
	cl := slip.FindPackage("common-lisp")
	f := slip.NewFunc("car", slip.List{nil}, cl)
	tt.Equal(t, "(car nil)", slip.ObjectString(f))
	tt.Equal(t, "common-lisp", f.GetPkg().Name)

	tt.Panic(t, func() { _ = slip.NewFunc("nothing", slip.List{}) })
	tt.Panic(t, func() { _ = slip.NewFunc("nothing:at-all", slip.List{}) })
}

func TestFunctionFind(t *testing.T) {
	cl := slip.FindPackage("common-lisp")
	f := slip.MustFindFunc("car", cl)
	tt.NotNil(t, f)

	tt.Panic(t, func() { _ = slip.MustFindFunc("nothing") })
}

func TestFunctionApply(t *testing.T) {
	scope := slip.NewScope()
	f := slip.NewFunc("car", slip.List{})
	result := f.Apply(scope, slip.List{slip.List{slip.Fixnum(7)}}, 0)
	tt.Equal(t, "7", slip.ObjectString(result))
}

func TestFunctionPkgApply(t *testing.T) {
	scope := slip.NewScope()
	f := slip.NewFunc("cl:car", slip.List{})
	result := f.Apply(scope, slip.List{slip.List{slip.Fixnum(7)}}, 0)
	tt.Equal(t, "7", slip.ObjectString(result))
}

func TestFunctionPkg2Apply(t *testing.T) {
	scope := slip.NewScope()
	f := slip.NewFunc("cl::car", slip.List{})
	result := f.Apply(scope, slip.List{slip.List{slip.Fixnum(7)}}, 0)
	tt.Equal(t, "7", slip.ObjectString(result))
}

func TestFunctionEvalArg(t *testing.T) {
	scope := slip.NewScope()
	defer func() { slip.CurrentPackage.Remove("x") }()

	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setq", slip.List{slip.Symbol("x"), slip.Fixnum(7)}),
		String: "(setq x 7)",
		Simple: []interface{}{"setq", "x", 7},
		Eval:   slip.Fixnum(7),
	}).Test(t)
	tt.Equal(t, slip.Fixnum(7), scope.Get(slip.Symbol("x")))
}

func TestFunctionExport(t *testing.T) {
	scope := slip.NewScope()
	defer func() { slip.CurrentPackage = &slip.UserPkg }()
	xpkg := slip.DefPackage("exported-test", []string{}, "testing")
	slip.CurrentPackage = xpkg
	slip.CurrentPackage.Use(&slip.CLPkg)
	_ = slip.ReadString(`(defun private-func () 'private)`).Eval(scope, nil)
	slip.CurrentPackage = &slip.UserPkg
	result := slip.ReadString(`(exported-test::private-func)`).Eval(scope, nil)
	tt.Equal(t, "private", slip.ObjectString(result))

	// TBD tt.Panic(t, func() { _ = slip.ReadString(`(exported-test:private-func)`).Eval(scope, nil) })

	xpkg.Export("private-func")
	result = slip.ReadString(`(exported-test:private-func)`).Eval(scope, nil)
	tt.Equal(t, "private", slip.ObjectString(result))

	xpkg.Unexport("private-func")
	// TBD tt.Panic(t, func() { _ = slip.ReadString(`(exported-test:private-func)`).Eval(scope, nil) })
}

func TestFunctionExportNested(t *testing.T) {
	scope := slip.NewScope()
	defer func() { slip.CurrentPackage = &slip.UserPkg }()
	xpkg := slip.DefPackage("xpack-test", []string{}, "testing")
	slip.CurrentPackage = xpkg
	slip.CurrentPackage.Use(&slip.CLPkg)
	_ = slip.ReadString(`(defun private-child () 3)`).Eval(scope, nil)
	// _ = slip.ReadString(`(defun private-parent () (private-child))`).Eval(scope, nil)
	_ = slip.ReadString(`(defun private-parent () (+ (private-child) 4))`).Eval(scope, nil)

	slip.CurrentPackage = &slip.UserPkg
	result := slip.ReadString(`(xpack-test::private-parent)`).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(7), result)

	xpkg.Export("private-parent")
	result = slip.ReadString(`(xpack-test:private-parent)`).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(7), result)
}
