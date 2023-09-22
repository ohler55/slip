// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"runtime"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRequireLoadPath(t *testing.T) {
	// TBD remove this when plugins work on macOS.
	if runtime.GOOS == "darwin" && runtime.GOARCH == "arm64" {
		return
	}
	slip.CurrentPackage.Set("*package-load-path*", slip.String("testplugin"))
	tf := sliptest.Function{
		Source: `(require 'testplugin)`,
		Expect: "",
	}
	tf.Test(t)
	result := slip.CompileString("(plug)").Eval(slip.NewScope(), 0)
	tt.Equal(t, `"Plugged it"`, slip.ObjectString(result))
}

func TestRequirePathArg(t *testing.T) {
	slip.CurrentPackage.Set("*package-load-path*", nil)
	(&sliptest.Function{
		Source: `(require "testplugin" "not-a-good-path")`,
		Panics: true,
	}).Test(t)
}

func TestRequireArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(require 'testplugin "not-a-good-path" t)`,
		Panics: true,
	}).Test(t)
}

func TestRequireBadName(t *testing.T) {
	(&sliptest.Function{
		Source: `(require t)`,
		Panics: true,
	}).Test(t)
}

func TestRequireBadPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(require "testplugin" '("testplugin" t))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(require "testplugin" t)`,
		Panics: true,
	}).Test(t)
}

func TestRequireBadLoadPath(t *testing.T) {
	slip.CurrentPackage.Set("*package-load-path*", slip.List{slip.String("testplugin"), slip.True})
	(&sliptest.Function{
		Source: `(require "testplugin")`,
		Panics: true,
	}).Test(t)
	slip.CurrentPackage.Set("*package-load-path*", slip.True)
	(&sliptest.Function{
		Source: `(require "testplugin")`,
		Panics: true,
	}).Test(t)
}

func TestRequireBadPkg(t *testing.T) {
	(&sliptest.Function{
		Source: `(require "bad" "testplugin")`,
		Panics: true,
	}).Test(t)
}
