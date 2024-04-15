// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSymbolPackageFunction(t *testing.T) {
	(&sliptest.Function{
		Source: `(symbol-package 'car)`,
		Expect: "#<package common-lisp>",
	}).Test(t)
}

func TestSymbolPackageVariable(t *testing.T) {
	(&sliptest.Function{
		Source: `(symbol-package '*print-level*)`,
		Expect: "#<package common-lisp>",
	}).Test(t)
}

func TestSymbolPackageWithPkg(t *testing.T) {
	(&sliptest.Function{
		Source: `(symbol-package 'cl:*print-level*)`,
		Expect: "#<package common-lisp>",
	}).Test(t)
}

func TestSymbolPackageNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(symbol-package t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSymbolPackageNotBound(t *testing.T) {
	(&sliptest.Function{
		Source: `(symbol-package 'sym-pak-unbound)`,
		Expect: "#<package common-lisp-user>",
	}).Test(t)
}
