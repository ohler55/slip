// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDocumentationVariable(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (doc doc2)
                  (defvar quux 3 "quack")
                  (setq doc (documentation 'quux 'variable))
                  (setf (documentation 'quux 'variable) "quick")
                  (setq doc2 (documentation 'quux 'variable))
                  (makunbound 'quux)
                  (list doc doc2))`,
		Expect: `("quack" "quick")`,
	}).Test(t)
}

func TestDocumentationFunction(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (doc doc2)
                  (defun quux () "quack" nil)
                  (setq doc (documentation 'quux 'function))
                  (setf (documentation 'quux 'function) "quick")
                  (setq doc2 (documentation 'quux 'function))
                  (fmakunbound 'quux)
                  (list doc doc2))`,
		Expect: `("quack" "quick")`,
	}).Test(t)
}

func TestDocumentationPackage(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (doc)
                  (setf (documentation *cl* t) "packer")
                  (setq doc (documentation *cl* t))
                  doc)`,
		Expect: `"packer"`,
	}).Test(t)
}

func TestDocumentationConstant(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (doc)
                  (setf (documentation 'pi 'constant) "pie")
                  (setq doc (documentation 'pi 'constant))
                  doc)`,
		Expect: `"pie"`,
	}).Test(t)
}

func TestDocumentationTypeBuiltIn(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (doc)
                  (setf (documentation 'fixnum 'type) "number")
                  (setq doc (documentation 'fixnum 'type))
                  doc)`,
		Expect: `"number"`,
	}).Test(t)
}

func TestDocumentationTypeFlavor(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (doc)
                  (setf (documentation 'vanilla-flavor 'type) "vanilla")
                  (setq doc (documentation 'vanilla-flavor 'type))
                  doc)`,
		Expect: `"vanilla"`,
	}).Test(t)
}

func TestDocumentationSymbolT(t *testing.T) {
	(&sliptest.Function{
		Source:    `(documentation 'foo t)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (documentation 'foo t) "x")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestDocumentationBadDocType(t *testing.T) {
	(&sliptest.Function{
		Source:    `(documentation 'foo 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(documentation 'foo 'bad)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (documentation 'foo 7) "x")`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (documentation 'foo 'bad) "x")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestDocumentationSetfNotString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(setf (documentation 'foo 'variable) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDocumentationUnsupported(t *testing.T) {
	(&sliptest.Function{
		Source:    `(documentation 7 t)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (documentation 7 t) "x")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
