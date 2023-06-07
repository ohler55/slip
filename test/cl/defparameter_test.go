// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDefparameterBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(defparameter *defparameter-test-basic*)`,
		Expect: `*defparameter-test-basic*`,
	}).Test(t)
	val, has := slip.CurrentPackage.Get("*defparameter-test-basic*")
	tt.Nil(t, val)
	tt.Equal(t, true, has)

	(&sliptest.Function{
		Source: `(defparameter *defparameter-test-basic* 7)`,
		Expect: `*defparameter-test-basic*`,
	}).Test(t)
	val, _ = slip.CurrentPackage.Get("*defparameter-test-basic*")
	tt.Equal(t, slip.Fixnum(7), val)
}

func TestDefparameterInitial(t *testing.T) {
	(&sliptest.Function{
		Source: `(defparameter *defparameter-test-initial* 7)`,
		Expect: `*defparameter-test-initial*`,
	}).Test(t)
	val, has := slip.CurrentPackage.Get("*defparameter-test-initial*")
	tt.Equal(t, slip.Fixnum(7), val)
	tt.Equal(t, true, has)
}

func TestDefparameterDoc(t *testing.T) {
	(&sliptest.Function{
		Source: `(defparameter *defparameter-test-doc* 7 "Documentation for test.")`,
		Expect: `*defparameter-test-doc*`,
	}).Test(t)
	val, has := slip.CurrentPackage.Get("*defparameter-test-doc*")
	tt.Equal(t, slip.Fixnum(7), val)
	tt.Equal(t, true, has)

	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(describe '*defparameter-test-doc*)`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, `common-lisp-user:*defparameter-test-doc*
  [symbol]

*defparameter-test-doc* names a fixnum:
  Documentation:
    Documentation for test.
  Value = 7
`, out.String())
}

func TestDefparameterArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(defparameter *defparameter-test-initial* 7 "xxx" t)`,
		Panics: true,
	}).Test(t)
}

func TestDefparameterBadName(t *testing.T) {
	(&sliptest.Function{
		Source: `(defparameter t)`,
		Panics: true,
	}).Test(t)
}

func TestDefparameterBadDoc(t *testing.T) {
	(&sliptest.Function{
		Source: `(defparameter bad-doc nil t)`,
		Panics: true,
	}).Test(t)
}
