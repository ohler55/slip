// Copyright (c) 2026, Peter Ohler, All rights reserved.

package test

import (
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFileChecksumOk(t *testing.T) {
	sum := slip.FileChecksum("testdata/app/quux.lisp")

	tt.Equal(t, "a51bdc2eaef1ee889fd3ac08f6a1127e2018f1b7f671be2ed3965c19f1056064", sum)
}

func TestFileChecksumNoFile(t *testing.T) {
	tt.Panic(t, func() { _ = slip.FileChecksum("testdata/nothing.lisp") })
}

func TestCoverageEmpty(t *testing.T) {
	slip.StartCoverage()
	slip.StopCoverage()

	covfile := "testdata/cov.lisp"
	os.RemoveAll(covfile)
	slip.WriteCoverage(covfile)
	cov, err := os.ReadFile(covfile)
	tt.Nil(t, err)
	tt.Equal(t, "(())\n", string(cov))
}

func TestCoverageSimple(t *testing.T) {
	slip.StartCoverage()
	scope := slip.NewScope()

	_ = slip.CompileString(`(load "testdata/cov-test2.lisp")`, scope).Eval(scope, 0)
	_ = slip.CompileString(`(load "testdata/cov-test.lisp")`, scope).Eval(scope, 0)

	slip.StopCoverage()

	covfile := "testdata/cov.lisp"
	os.RemoveAll(covfile)
	slip.WriteCoverage(covfile)
	cov, err := os.ReadFile(covfile)
	tt.Nil(t, err)

	tt.Equal(t, `/testdata\/cov-test.lisp" 1 1 2 11 1\)/`, string(cov))
	tt.Equal(t, `/testdata\/cov-test.lisp" 2 4 2 10 1\)/`, string(cov))
	tt.Equal(t, `/testdata\/cov-test2.lisp" 1 1 1 14 1\)/`, string(cov))

	slip.ResetCoverage(false)
	slip.WriteCoverage(covfile)
	cov, err = os.ReadFile(covfile)
	tt.Nil(t, err)
	tt.Equal(t, `/testdata\/cov-test.lisp" 1 1 2 11 0\)/`, string(cov))

	slip.ResetCoverage(true)
	slip.WriteCoverage(covfile)
	cov, err = os.ReadFile(covfile)
	tt.Nil(t, err)
	tt.Equal(t, "(())\n", string(cov))
}

func TestCoverageError(t *testing.T) {
	slip.StartCoverage()
	slip.StopCoverage()

	tt.Panic(t, func() { slip.WriteCoverage("testdata/nodir/cov.lisp") })
}

func TestCoverageInTest(t *testing.T) {
	defer func() {
		slip.ResetCoverage(true)
		slip.StopCoverage()
	}()
	slip.ResetCoverage(true)
	slip.StartCoverage()
	(&sliptest.Function{
		Source: `(progn
                   (+ 1 2)
                   (coverage-report nil))`,
		Expect: `"(("TestCoverageInTest" nil)
 ("TestCoverageInTest" 0 0 2 41 0)
 ("TestCoverageInTest" 1 20 1 26 1)
 ("TestCoverageInTest" 2 20 2 40 1))
"`,
	}).Test(t)
}

func TestCoverageDefun(t *testing.T) {
	defer func() {
		slip.ResetCoverage(true)
		slip.StopCoverage()
		slip.CurrentPackage.Remove("cov-fun")

	}()
	slip.ResetCoverage(true)
	slip.StartCoverage()
	(&sliptest.Function{
		Source: `(progn
                   (defun cov-fun (x) (+ 2 x))
                   (cov-fun 3)
                   (coverage-report nil))`,
		Expect: `"(("TestCoverageDefun" nil)
 ("TestCoverageDefun" 0 0 3 41 0)
 ("TestCoverageDefun" 1 20 1 46 1)
 ("TestCoverageDefun" 1 39 1 45 1)
 ("TestCoverageDefun" 2 20 2 30 1)
 ("TestCoverageDefun" 3 20 3 40 1))
"`,
	}).Test(t)
}

func TestCoverageMethod(t *testing.T) {
	defer func() {
		slip.ResetCoverage(true)
		slip.StopCoverage()
		undefFlavor("cov-flavor")

	}()
	slip.ResetCoverage(true)
	slip.StartCoverage()
	(&sliptest.Function{
		Source: `(progn
                   (defflavor cov-flavor () ())
                   (defmethod (cov-flavor :ok) () t)
                   (send (make-instance 'cov-flavor) :ok)
                   (coverage-report nil))`,
		Expect: `"(("TestCoverageMethod" nil)
 ("TestCoverageMethod" 0 0 4 41 0)
 ("TestCoverageMethod" 1 20 1 47 1)
 ("TestCoverageMethod" 2 20 2 52 1)
 ("TestCoverageMethod" 3 20 3 57 1)
 ("TestCoverageMethod" 3 26 3 52 1)
 ("TestCoverageMethod" 4 20 4 40 1))
"`,
	}).Test(t)
}
