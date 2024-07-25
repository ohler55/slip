// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWriteByteOkay(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString("(write-byte 65 out)").Eval(scope, nil)

	tt.Equal(t, slip.Fixnum(65), result)
	tt.Equal(t, "A", out.String())
}

func TestWriteByteNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(write-byte 65 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWriteByteNotByte(t *testing.T) {
	(&sliptest.Function{
		Source:    `(write-byte t *standard-output*)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWriteByteWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(write-byte 65 out)`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}
