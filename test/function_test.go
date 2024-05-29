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

func TestFunctionHierarchy(t *testing.T) {
	fi := &slip.FuncInfo{
		Name:   "dummy",
		Create: func(args slip.List) slip.Object { return nil },
		Pkg:    &slip.UserPkg,
	}
	tt.Equal(t, []slip.Symbol{slip.FunctionSymbol, slip.TrueSymbol}, fi.Hierarchy())
}

func TestFunctionFind(t *testing.T) {
	cl := slip.FindPackage("common-lisp")
	f := slip.MustFindFunc("car", cl)
	tt.NotNil(t, f)
	tt.Equal(t, []slip.Symbol{slip.BuiltInSymbol, slip.TrueSymbol}, f.Hierarchy())

	tt.Panic(t, func() { _ = slip.MustFindFunc("nothing", &slip.CLPkg) })
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

	tt.Panic(t, func() { _ = slip.ReadString(`(exported-test:private-func)`).Eval(scope, nil) })

	xpkg.Export("private-func")
	result = slip.ReadString(`(exported-test:private-func)`).Eval(scope, nil)
	tt.Equal(t, "private", slip.ObjectString(result))

	xpkg.Unexport("private-func")
	tt.Panic(t, func() { _ = slip.ReadString(`(exported-test:private-func)`).Eval(scope, nil) })
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

func TestListToFunc(t *testing.T) {
	obj := slip.ListToFunc(
		slip.NewScope(),
		slip.List{
			slip.List{slip.Symbol("lambda"), slip.List{},
				slip.List{slip.Symbol("+"), slip.Fixnum(1), slip.Fixnum(2)},
			},
		},
		0)
	tt.SameType(t, &slip.Dynamic{}, obj)

	obj = slip.ListToFunc(slip.NewScope(), slip.List{}, 0)
	tt.Nil(t, obj)
}

func TestFunctionCaller(t *testing.T) {
	f := slip.NewFunc("car", slip.List{nil})
	tt.NotNil(t, f)
	tt.Equal(t, f, f.Caller())
}

func TestMustBeString(t *testing.T) {
	str := slip.MustBeString(slip.String("one"), "test")
	tt.Equal(t, "one", str)

	str = slip.MustBeString(slip.Symbol("two"), "test")
	tt.Equal(t, "two", str)

	str = slip.MustBeString(slip.Symbol(":three"), "test")
	tt.Equal(t, "three", str)

	tt.Panic(t, func() { _ = slip.MustBeString(slip.True, "test") })
}

func TestGetArgsKeyValue(t *testing.T) {
	scope := slip.NewScope()
	args := slip.ReadString("(list :one 1 :two 2)").Eval(scope, nil).(slip.List)

	val, has := slip.GetArgsKeyValue(args, slip.Symbol(":one"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Fixnum(1), val)

	_, has = slip.GetArgsKeyValue(args, slip.Symbol(":three"))
	tt.Equal(t, false, has)

	tt.Panic(t, func() {
		slip.GetArgsKeyValue(slip.ReadString("(list t 1)").Eval(scope, nil).(slip.List), slip.Symbol(":x"))
	})

	tt.Panic(t, func() {
		slip.GetArgsKeyValue(slip.ReadString("(list :x 1 :y)").Eval(scope, nil).(slip.List), slip.Symbol(":y"))
	})
}

func TestCompileList(t *testing.T) {
	f := slip.CompileList(slip.List{slip.Symbol("no-fun")})
	tt.Equal(t, []slip.Symbol{slip.FunctionSymbol, slip.TrueSymbol}, f.Hierarchy())

	tt.Equal(t, "(no-fun)", slip.ObjectString(f))
	// Should be unbound since it was not defun-ed yet.
	tt.Panic(t, func() { _ = f.Eval(slip.NewScope(), 0) })
}
