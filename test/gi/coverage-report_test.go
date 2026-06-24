// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

// Just test the destinations. Content is checked with the reset-coverage
// test.

func TestCoverageReportString(t *testing.T) {
	(&sliptest.Function{
		Source: `(coverage-report nil)`,
		Expect: `"(())
"`,
	}).Test(t)
}

func TestCoverageReportStdout(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-output-to-string (s)
                   (let ((*standard-output* s))
                     (coverage-report t)))`,
		Expect: `"(())
"`,
	}).Test(t)
}

func TestCoverageReportStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-output-to-string (s)
                   (coverage-report s))`,
		Expect: `"(())
"`,
	}).Test(t)
}

func TestCoverageReportFile(t *testing.T) {
	defer func() { _ = os.RemoveAll("testdata/cov.lisp") }()
	(&sliptest.Function{
		Source: `(coverage-report "testdata/cov.lisp")`,
		Expect: "nil",
	}).Test(t)
	content, err := os.ReadFile("testdata/cov.lisp")
	tt.Nil(t, err)
	tt.Equal(t, "(())\n", string(content))
}

func TestCoverageReportBadDestination(t *testing.T) {
	(&sliptest.Function{
		Source:    `(coverage-report 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestCoverageReportWriteFail(t *testing.T) {
	scope := slip.NewScope()
	slip.SetVar(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(coverage-report out)`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}

func TestCoverageReportBadFile(t *testing.T) {
	(&sliptest.Function{
		Source:    `(coverage-report "testdata/comp.lisp/quux.lisp")`,
		PanicType: slip.FileErrorSymbol,
	}).Test(t)
}
