// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"bytes"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDefmacroBasic(t *testing.T) {
	scope := slip.NewScope()
	(&sliptest.Function{
		Scope:  scope,
		Source: "(defmacro mac (x) `(1+ ,x))",
		Expect: "mac",
	}).Test(t)
	result := slip.ReadString("(mac 3)", scope).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(4), result)
}

func TestDefmacroDup(t *testing.T) {
	var b bytes.Buffer
	scope := slip.NewScope()
	scope.Let("*error-output*", &slip.OutputStream{Writer: &b})
	_ = slip.ReadString("(defmacro mac2 (x) `(1+ ,x))", scope).Eval(scope, nil)
	_ = slip.ReadString("(defmacro mac2 (x) `(1+ ,x))", scope).Eval(scope, nil)
	tt.Equal(t, "WARNING: redefining common-lisp-user:mac2 in defmacro\n", b.String())
}

func TestDefmacroClosure(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(let ((y 3)) (defmacro clom (x) `(+ ,x ,y)))", scope).Eval(scope, nil)
	(&sliptest.Function{
		Source: "(clom 2)",
		Expect: "5",
	}).Test(t)
}

func TestDefmacroBadName(t *testing.T) {
	(&sliptest.Function{
		Source: "(defmacro 3 (x) `(1+ ,x))",
		Panics: true,
	}).Test(t)
}

func TestDefmacroLocked(t *testing.T) {
	(&sliptest.Function{
		Source: "(defmacro car (x) `(1+ ,x))",
		Panics: true,
	}).Test(t)
}
