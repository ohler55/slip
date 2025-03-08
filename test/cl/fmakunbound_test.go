// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFmakunboundOk(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(defun fmakunbound-test () t)", scope).Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(fmakunbound 'fmakunbound-test)`,
		Expect: "fmakunbound-test",
	}).Test(t)
	result := slip.ReadString("(fboundp 'fmakunbound-test)", scope).Eval(slip.NewScope(), nil)
	tt.Equal(t, nil, result)
}

func TestFmakunboundNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(fmakunbound 7)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
