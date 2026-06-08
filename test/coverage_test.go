// Copyright (c) 2026, Peter Ohler, All rights reserved.

package test

import (
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
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
}

func TestCoverageError(t *testing.T) {
	slip.StartCoverage()
	slip.StopCoverage()

	tt.Panic(t, func() { slip.WriteCoverage("testdata/nodir/cov.lisp") })
}
