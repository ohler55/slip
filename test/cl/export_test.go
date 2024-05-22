// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestExportSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(export 'cadr)`,
		Expect: "t",
	}).Test(t)
}

func TestExportString(t *testing.T) {
	(&sliptest.Function{
		Source: `(export "cadr")`,
		Expect: "t",
	}).Test(t)
}

func TestExportList(t *testing.T) {
	(&sliptest.Function{
		Source: `(export '(cadr "cdr"))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(export '(t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestExportFromPackage(t *testing.T) {
	(&sliptest.Function{
		Source: `(export 'cadr *cl*)`,
		Expect: "t",
	}).Test(t)
}

func TestExportBadSymbols(t *testing.T) {
	(&sliptest.Function{
		Source:    `(export t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
