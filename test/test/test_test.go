// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test_test

import (
	"bytes"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTestPass(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadEvalString(`(setq sweet (make-instance 'suite-flavor :name "sweet"))`, scope)
	sf := sliptest.Function{
		Scope: scope,
		Source: `(make-instance 'test-flavor
                   :name "toot"
                   :parent sweet
                   :forms '((+ 1 2)))`,
		Expect: "/<test-flavor [0-9a-f]+>/",
	}
	sf.Test(t)
	scope.Let(slip.Symbol("toot"), sf.Result)
	_ = slip.ReadEvalString(`(send toot :run)`, scope)
	result := slip.ReadEvalString(`(send toot :result)`, scope)
	tt.Equal(t, slip.Symbol(":pass"), result)
	_ = slip.ReadEvalString(`(send toot :report out)`, scope)
	tt.Equal(t, "/^  toot: .*PASS.*/", out.String())
}

func TestTestFail(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let(slip.Symbol("*trace-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadEvalString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((+ 1 (/ 2 0)))))`, scope)
	_ = slip.ReadEvalString(`(send toot :run :trace t)`, scope)
	result := slip.ReadEvalString(`(send toot :result)`, scope)
	tt.Equal(t, slip.Symbol(":fail"), result)
	_ = slip.ReadEvalString(`(send toot :report)`, scope)

	for _, seq := range []string{
		"0: (+ 1 (/ 2 0))\n",
		"  1: (/ 2 0)\n",
		"    2: (initialize-instance #<division-by-zero",
		"      3: (shared-initialize #<division-by-zero",
		"  1: (/ 2 0) => divide by zero\n",
		"0: (+ 1 (/ 2 0)) => divide by zero\n",
		`toot: FAIL
  divide by zero
toot: FAIL
`,
	} {
		tt.Equal(t, true, strings.Contains(out.String(), seq), seq)
	}
	out.Reset()
	_ = slip.ReadEvalString(`(send toot :reset)`, scope)
	_ = slip.ReadEvalString(`(send toot :report)`, scope)
	tt.Equal(t, `toot: SKIP
`, out.String())
}

func TestTestDocs(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)

	_ = slip.ReadEvalString(`(describe-method 'test-flavor :run)`, scope)
	tt.Equal(t, "/:run is a method of test-flavor/", out.String())

	out.Reset()
	_ = slip.ReadEvalString(`(describe-method 'test-flavor :result)`, scope)
	tt.Equal(t, "/:result is a method of test-flavor/", out.String())

	out.Reset()
	_ = slip.ReadEvalString(`(describe-method 'test-flavor :report)`, scope)
	tt.Equal(t, "/:report is a method of test-flavor/", out.String())

	out.Reset()
	_ = slip.ReadEvalString(`(describe-method 'test-flavor :reset)`, scope)
	tt.Equal(t, "/:reset is a method of test-flavor/", out.String())

	out.Reset()
	_ = slip.ReadEvalString(`(describe-method 'test-flavor :init)`, scope)
	tt.Equal(t, "/:init is a method of test-flavor/", out.String())
}

func TestTestVerbose(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadEvalString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((+ 1 (/ 2 0)))))`, scope)
	_ = slip.ReadEvalString(`(send toot :run :verbose t)`, scope)
	tt.Equal(t, `toot: FAIL
  divide by zero
  (/ 2 0)
  (+ 1 (/ 2 0))
`, out.String())

	out.Reset()
	_ = slip.ReadEvalString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((+ 1 (/ 3 2)))))`, scope)
	_ = slip.ReadEvalString(`(send toot :run :verbose t)`, scope)
	tt.Equal(t, "toot: PASS\n", out.String())
}

func TestTestReportBadWriter(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadEvalString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((/ 1 0))))`, scope)
	tt.Panic(t, func() { _ = slip.ReadEvalString(`(send toot :report t)`, scope) })
}

func TestTestRunPanics(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadEvalString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((/ 3 2))))`, scope)
	tt.Panic(t, func() { _ = slip.ReadEvalString(`(send toot :run t)`, scope) })
	tt.Panic(t, func() { _ = slip.ReadEvalString(`(send toot :run :verbose)`, scope) })
	tt.Panic(t, func() { _ = slip.ReadEvalString(`(send toot :run :bad t)`, scope) })
}

func TestTestInitPanic(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadEvalString(`(make-instance 'test-flavor
                                            :name "toot"
                                            :parent (make-instance 'vanilla-flavor)
                                            :forms '(nil))`, scope)
	})
}
