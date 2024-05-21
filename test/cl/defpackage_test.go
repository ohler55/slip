// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDefpackageSimple(t *testing.T) {
	defer func() {
		slip.RemovePackage(slip.FindPackage("defpack-test-1"))
	}()
	(&sliptest.Function{
		Source: `(defpackage 'defpack-test-1 (:nicknames pt1 "pt-1"))`,
		Validate: func(t *testing.T, v slip.Object) {
			tt.Equal(t, "#<package defpack-test-1>", slip.ObjectString(v))
			p := v.(*slip.Package)
			tt.Equal(t, "[pt1 pt-1]", pretty.SEN(p.Nicknames))
		},
	}).Test(t)
}

func TestDefpackageUse(t *testing.T) {
	defer func() {
		slip.RemovePackage(slip.FindPackage("defpack-test-5"))
		slip.RemovePackage(slip.FindPackage("defpack-test-4"))
		slip.RemovePackage(slip.FindPackage("defpack-test-3"))
		slip.RemovePackage(slip.FindPackage("defpack-test-2"))
	}()
	(&sliptest.Function{
		Source: `(progn
                  (defpackage 'defpack-test-2 (:nicknames pt2))
                  (defpackage 'defpack-test-3 (:nicknames pt3))
                  (defpackage 'defpack-test-4 (:nicknames pt4))
                  (defpackage 'defpack-test-5 (:use pt2 "pt3" pt4)))`,
		Validate: func(t *testing.T, v slip.Object) {
			tt.Equal(t, "#<package defpack-test-5>", slip.ObjectString(v))
			p5 := v.(*slip.Package)
			p2 := slip.FindPackage("pt2")
			tt.Equal(t, "#<package defpack-test-2>", slip.ObjectString(p2))

			tt.Equal(t, 3, len(p5.Uses))
			tt.Equal(t, "#<package defpack-test-2>", slip.ObjectString(p5.Uses[0]))
			tt.Equal(t, "#<package defpack-test-3>", slip.ObjectString(p5.Uses[1]))
			tt.Equal(t, "#<package defpack-test-4>", slip.ObjectString(p5.Uses[2]))
			tt.Equal(t, 1, len(p2.Users))
			tt.Equal(t, "#<package defpack-test-5>", slip.ObjectString(p2.Users[0]))
		},
	}).Test(t)
}

func TestDefpackageExport(t *testing.T) {
	defer func() {
		slip.RemovePackage(slip.FindPackage("defpack-test-6"))
	}()
	(&sliptest.Function{
		Source: `(defpackage 'defpack-test-6 (:export "fun1" "v2"))`,
		Validate: func(t *testing.T, v slip.Object) {
			scope := slip.NewScope()
			tt.Equal(t, "#<package defpack-test-6>", slip.ObjectString(v))
			p := v.(*slip.Package)
			vv := p.GetVarVal("v2")
			tt.NotNil(t, vv)
			tt.Equal(t, slip.Unbound, vv.Val)
			vv = p.GetVarVal("fun1")
			tt.NotNil(t, vv)
			tt.Equal(t, slip.Unbound, vv.Val)
			fi := slip.FindFunc("fun1", p)
			tt.Nil(t, fi)

			_ = slip.ReadString("(defvar defpack-test-6::v2 3)").Eval(scope, nil)
			vv = p.GetVarVal("v2")
			tt.NotNil(t, vv)
			tt.Equal(t, slip.Fixnum(3), vv.Val)

			_ = slip.ReadString("(defun defpack-test-6::fun1 () 7)").Eval(scope, nil)
			vv = p.GetVarVal("fun1")
			tt.Nil(t, vv)
			fi = slip.FindFunc("fun1", p)
			tt.NotNil(t, fi)
			tt.Equal(t, true, fi.Export)
		},
	}).Test(t)
}

func TestDefpackageExists(t *testing.T) {
	(&sliptest.Function{
		Source:    `(defpackage 'bag)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(defpackage 'defpack-test-6 (:nicknames "bag"))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestDefpackageBadNicknames(t *testing.T) {
	(&sliptest.Function{
		Source:    `(defpackage 'test-defpack-6 (:nicknames t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDefpackageUseNotFound(t *testing.T) {
	(&sliptest.Function{
		Source:    `(defpackage 'test-defpack-6 (:use quux))`,
		PanicType: slip.PackageErrorSymbol,
	}).Test(t)
}

func TestDefpackageBadOption(t *testing.T) {
	(&sliptest.Function{
		Source:    `(defpackage 'test-defpack-6 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
