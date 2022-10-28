// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFormatStream(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString(`(format out "abc")`).Eval(scope)

	tt.Equal(t, nil, result)
	tt.Equal(t, "abc", out.String())
}

func TestFormatString(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "abc")`,
		Expect: `"abc"`,
	}).Test(t)
}

func TestFormatA(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~1,2,3,'-:@A" 12)`,
		Expect: `"---12"`,
	}).Test(t)
}

func TestFormatTilde(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~~")`,
		Expect: `"~"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~3~")`,
		Expect: `"~~~"`,
	}).Test(t)
}
