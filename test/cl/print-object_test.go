// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"bytes"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPrintObjectOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (print-object 'cymbal out)
                  (get-output-stream-string out))`,
		Expect: `"cymbal"`,
	}).Test(t)
}

func TestPrintObjectWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(print-object 'cymbal out)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestPrintObjectNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(print-object 'cymbal t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestPrintObjectStructWithPrintFunction(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadEvalString(`(defstruct (pospf (:print-function (lambda (obj str depth) (princ "x" str)))) x)`,
		scope)
	var b bytes.Buffer
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &b})
	_ = slip.ReadEvalString("(print-object (make-pospf :x 1) out)", scope)
	tt.Equal(t, "x", b.String())
}
