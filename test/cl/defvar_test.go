// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDefvarBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(defvar *defvar-test-basic*)`,
		Expect: `*defvar-test-basic*`,
	}).Test(t)
	val, has := slip.CurrentPackage.Get("*defvar-test-basic*")
	tt.Equal(t, slip.Unbound, val)
	tt.Equal(t, true, has)

	(&sliptest.Function{
		Source: `(defvar *defvar-test-basic* 7)`,
		Expect: `*defvar-test-basic*`,
	}).Test(t)
	val, _ = slip.CurrentPackage.Get("*defvar-test-basic*")
	tt.Equal(t, slip.Fixnum(7), val)
}

func TestDefvarInitial(t *testing.T) {
	(&sliptest.Function{
		Source: `(defvar *defvar-test-initial* 7)`,
		Expect: `*defvar-test-initial*`,
	}).Test(t)
	val, has := slip.CurrentPackage.Get("*defvar-test-initial*")
	tt.Equal(t, slip.Fixnum(7), val)
	tt.Equal(t, true, has)

	(&sliptest.Function{
		Source: `(defvar *defvar-test-initial* 8)`,
		Expect: `*defvar-test-initial*`,
	}).Test(t)
	val, has = slip.CurrentPackage.Get("*defvar-test-initial*")
	tt.Equal(t, slip.Fixnum(7), val)
	tt.Equal(t, true, has)
}

func TestDefvarDoc(t *testing.T) {
	(&sliptest.Function{
		Source: `(defvar *defvar-test-doc* 7 "Documentation for test.")`,
		Expect: `*defvar-test-doc*`,
	}).Test(t)
	val, has := slip.CurrentPackage.Get("*defvar-test-doc*")
	tt.Equal(t, slip.Fixnum(7), val)
	tt.Equal(t, true, has)

	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(describe '*defvar-test-doc*)`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, `*defvar-test-doc*
  [symbol]

*defvar-test-doc* names a fixnum:
  Documentation:
    Documentation for test.
  Value = 7
`, out.String())
}

func TestDefvarArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(defvar *defvar-test-initial* 7 "xxx" t)`,
		Panics: true,
	}).Test(t)
}

func TestDefvarBadName(t *testing.T) {
	(&sliptest.Function{
		Source: `(defvar t)`,
		Panics: true,
	}).Test(t)
}

func TestDefvarBadDoc(t *testing.T) {
	(&sliptest.Function{
		Source: `(defvar bad-doc nil t)`,
		Panics: true,
	}).Test(t)
}
