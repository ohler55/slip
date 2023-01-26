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

	tt.Panic(t, func() { _ = slip.NewFunc("nothing", slip.List{}) })
}

func TestFunctionFind(t *testing.T) {
	cl := slip.FindPackage("common-lisp")
	f := slip.FindFunc("car", cl)
	tt.NotNil(t, f)

	tt.Panic(t, func() { _ = slip.FindFunc("nothing") })
}

func TestFunctionApply(t *testing.T) {
	scope := slip.NewScope()
	f := slip.NewFunc("car", slip.List{}).(slip.Funky)
	result := f.Apply(scope, slip.List{slip.List{slip.Fixnum(7)}}, 0)
	tt.Equal(t, "7", slip.ObjectString(result))
}

func TestFunctionPkgApply(t *testing.T) {
	scope := slip.NewScope()
	f := slip.NewFunc("cl:car", slip.List{}).(slip.Funky)
	result := f.Apply(scope, slip.List{slip.List{slip.Fixnum(7)}}, 0)
	tt.Equal(t, "7", slip.ObjectString(result))
}

func TestFunctionPkg2Apply(t *testing.T) {
	scope := slip.NewScope()
	f := slip.NewFunc("cl::car", slip.List{}).(slip.Funky)
	result := f.Apply(scope, slip.List{slip.List{slip.Fixnum(7)}}, 0)
	tt.Equal(t, "7", slip.ObjectString(result))
}

func TestFunctionEvalArg(t *testing.T) {
	scope := slip.NewScope()
	defer func() { slip.CurrentPackage.Remove("x") }()

	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setq", slip.List{slip.Fixnum(7), slip.Symbol("x")}),
		String: "(setq x 7)",
		Simple: []interface{}{"setq", "x", 7},
		Eval:   slip.Fixnum(7),
	}).Test(t)
	tt.Equal(t, slip.Fixnum(7), scope.Get(slip.Symbol("x")))
}
