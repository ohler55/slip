// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRequireLoadPath(t *testing.T) {
	slip.CurrentPackage.Set("*package-load-path*", slip.String("testplugin"))
	tf := sliptest.Function{
		Source: `(require 'testplugin)`,
		Expect: "",
	}
	tf.Test(t)
	scope := slip.NewScope()
	result := slip.CompileString("(plug)", scope).Eval(slip.NewScope(), 0)
	tt.Equal(t, `"Plugged it"`, slip.ObjectString(result))
}

func TestRequirePathArg(t *testing.T) {
	slip.CurrentPackage.Set("*package-load-path*", nil)
	(&sliptest.Function{
		Source:    `(require "testplugin" "~/not-a-good-path")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestRequireArgCount(t *testing.T) {
	(&sliptest.Function{
		Source:    `(require 'testplugin "not-a-good-path" t)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestRequireBadName(t *testing.T) {
	(&sliptest.Function{
		Source:    `(require t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestRequireBadPath(t *testing.T) {
	(&sliptest.Function{
		Source:    `(require "testplugin" '("testplugin" t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(require "testplugin" t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestRequireBadLoadPath(t *testing.T) {
	slip.CurrentPackage.Set("*package-load-path*", slip.List{slip.String("testplugin"), slip.True})
	(&sliptest.Function{
		Source:    `(require "testplugin")`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	slip.CurrentPackage.Set("*package-load-path*", slip.True)
	(&sliptest.Function{
		Source:    `(require "testplugin")`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestRequireBadPkg(t *testing.T) {
	(&sliptest.Function{
		Source:    `(require "bad" "testplugin")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestRequireLoadPathLisp(t *testing.T) {
	slip.CurrentPackage.Set("*package-load-path*", slip.String("testdata"))
	tf := sliptest.Function{
		Source: `(require 'load-me)`,
		Expect: "",
	}
	tf.Test(t)
	scope := slip.NewScope()
	result := slip.CompileString("load-test-me-too", scope).Eval(slip.NewScope(), 0)
	tt.Equal(t, "5", slip.ObjectString(result))
}

func TestRequireLisp(t *testing.T) {
	slip.CurrentPackage.Set("*package-load-path*", nil)
	tf := sliptest.Function{
		Source: `(require "load-me.lisp" "testdata")`,
		Expect: "",
	}
	tf.Test(t)
	scope := slip.NewScope()
	result := slip.CompileString("load-test-me-too", scope).Eval(slip.NewScope(), 0)
	tt.Equal(t, "5", slip.ObjectString(result))
}

func TestRequireNotReadable(t *testing.T) {
	os.Chmod("testdata/not-readable.lisp", 0322)
	defer func() { _ = os.Chmod("testdata/not-readable.lisp", 0644) }()
	(&sliptest.Function{
		Source:    `(require "not-readable.lisp" "testdata")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
