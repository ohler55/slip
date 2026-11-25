// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPrin1ToStringBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(prin1-to-string #\A)`,
		Expect: `"#\A"`,
	}).Test(t)
}

func TestPrin1ToStringStructWithPrintFunction(t *testing.T) {
	scope := slip.NewScope()
	slip.ReadEvalString(`(defstruct (p12spf (:print-function (lambda (obj str depth) (princ "x" str)))) x)`,
		scope)
	out := slip.ReadEvalString("(prin1-to-string (make-p12spf :x 1))", scope)
	tt.Equal(t, `"x"`, out.String())
}
