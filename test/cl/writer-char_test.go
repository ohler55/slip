// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWriteCharOkay(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString(`(write-char #\A out)`).Eval(scope, nil)

	tt.Equal(t, slip.Character('A'), result)
	tt.Equal(t, "A", out.String())
}

func TestWriteCharNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(write-char #\A t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWriteCharNotChar(t *testing.T) {
	(&sliptest.Function{
		Source:    `(write-char t *standard-output*)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWriteCharWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(write-char #\A out)`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}
