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
	_ = slip.ReadEvalString("(defun fmakunbound-test () t)", slip.NewScope())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(fmakunbound 'fmakunbound-test)`,
		Expect: "fmakunbound-test",
	}).Test(t)
	result := slip.ReadEvalString("(fboundp 'fmakunbound-test)", slip.NewScope())
	tt.Equal(t, nil, result)
}

func TestFmakunboundNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(fmakunbound 7)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
