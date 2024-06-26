// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPrintUnreadableObjectOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (print-unreadable-object *package* out)
                  (get-output-stream-string out))`,
		Expect: `/"#<package [0-9a-f]+>"/`,
	}).Test(t)
}

func TestPrintUnreadableObjectWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(print-unreadable-object 'cymbal out)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestPrintUnreadableObjectNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(print-unreadable-object 'cymbal t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
