// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"fmt"
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
	slip.ReadString(`(defstruct (p1wspf (:print-function (lambda (obj str depth) (princ "x" str)))) x)`,
		scope).Eval(scope, nil)
	out := slip.ReadString("(prin1-to-string (make-p1wspf :x 1))", scope).Eval(scope, nil)
	fmt.Printf("*** %q\n", out)
	tt.Equal(t, `"x"`, out.String())
}
