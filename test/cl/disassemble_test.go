// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDisassembleBuiltIn(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	scope.Let("*print-right-margin*", slip.Fixnum(80))

	(&sliptest.Function{
		Scope:  scope,
		Source: `(disassemble 'list)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, `(defun list (&rest objects)
  "list returns a list of all the objects."
  ...)
`, out.String())
}

func TestDisassembleLambda(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	scope.Let("*print-right-margin*", slip.Fixnum(80))

	(&sliptest.Function{
		Scope:  scope,
		Source: `(disassemble (lambda (x y) (+ x y)))`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, `(lambda (x y)
  (+ x y))
`, out.String())
}

func TestDisassembleDefun(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	scope.Let("*print-right-margin*", slip.Fixnum(80))

	(&sliptest.Function{
		Scope: scope,
		Source: `(defun quux (x) (1+ x))
                 (progn
                  (disassemble 'quux)
                  (fmakunbound 'quux))`,
		Expect: "quux",
	}).Test(t)
	tt.Equal(t, `(defun quux (x)
  (1+ x))
`, out.String())
}

func TestDisassembleDefmacro(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	scope.Let("*print-right-margin*", slip.Fixnum(80))

	(&sliptest.Function{
		Scope: scope,
		Source: `(defmacro quux (x) (1+ x))
                 (progn
                  (disassemble 'quux)
                  (fmakunbound 'quux))`,
		Expect: "quux",
	}).Test(t)
	tt.Equal(t, `(defmacro quux (x)
  (1+ x))
`, out.String())
}

func TestDisassembleNotFunc(t *testing.T) {
	(&sliptest.Function{
		Source:    `(disassemble 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(disassemble 'not-a-function)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
