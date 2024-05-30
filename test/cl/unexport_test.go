// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestUnexportSymbol(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(export 'ux1)").Eval(scope, nil)
	(&sliptest.Function{
		Source: `(unexport 'ux1)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(unexport 'ux1 *common-lisp-user*)`,
		Expect: "t",
	}).Test(t)
}

func TestUnexportString(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(export "ux2")`).Eval(scope, nil)
	(&sliptest.Function{
		Source: `(unexport "ux2")`,
		Expect: "t",
	}).Test(t)
}

func TestUnexportList(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(export "ux3")`).Eval(scope, nil)
	(&sliptest.Function{
		Source: `(unexport '(ux3 "ux4"))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(unexport '(t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestUnexportBadSymbols(t *testing.T) {
	(&sliptest.Function{
		Source:    `(unexport t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
