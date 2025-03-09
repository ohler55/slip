// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test_test

import (
	"bytes"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTestPass(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(setq sweet (make-instance 'suite-flavor :name "sweet"))`, scope).Eval(scope, nil)
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
	_ = slip.ReadString(`(send toot :run)`, scope).Eval(scope, nil)
	result := slip.ReadString(`(send toot :result)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.Symbol(":pass"), result)
	_ = slip.ReadString(`(send toot :report out)`, scope).Eval(scope, nil)
	tt.Equal(t, "/^  toot: .*PASS.*/", out.String())
}

func TestTestFail(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let(slip.Symbol("*trace-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((+ 1 (/ 2 0)))))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :trace t)`, scope).Eval(scope, nil)
	result := slip.ReadString(`(send toot :result)`, scope).Eval(scope, nil)
	tt.Equal(t, slip.Symbol(":fail"), result)
	_ = slip.ReadString(`(send toot :report)`, scope).Eval(scope, nil)
	tt.Equal(t, `0: (+ 1 (/ 2 0))
  1: (/ 2 0)
  1: (/ 2 0) => divide by zero
0: (+ 1 (/ 2 0)) => divide by zero
toot: FAIL
  divide by zero
toot: FAIL
`, out.String())

	out.Reset()
	_ = slip.ReadString(`(send toot :reset)`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :report)`, scope).Eval(scope, nil)
	tt.Equal(t, `toot: SKIP
`, out.String())
}

func TestTestDocs(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)

	_ = slip.ReadString(`(describe-method 'test-flavor :run)`, scope).Eval(scope, nil)
	tt.Equal(t, "/:run is a method of test-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'test-flavor :result)`, scope).Eval(scope, nil)
	tt.Equal(t, "/:result is a method of test-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'test-flavor :report)`, scope).Eval(scope, nil)
	tt.Equal(t, "/:report is a method of test-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'test-flavor :reset)`, scope).Eval(scope, nil)
	tt.Equal(t, "/:reset is a method of test-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'test-flavor :init)`, scope).Eval(scope, nil)
	tt.Equal(t, "/:init is a method of test-flavor/", out.String())
}

func TestTestVerbose(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((+ 1 (/ 2 0)))))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :verbose t)`, scope).Eval(scope, nil)
	tt.Equal(t, `toot: FAIL
  divide by zero
  (/ 2 0)
  (+ 1 (/ 2 0))
`, out.String())

	out.Reset()
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((+ 1 (/ 3 2)))))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :verbose t)`, scope).Eval(scope, nil)
	tt.Equal(t, "toot: PASS\n", out.String())
}

func TestTestReportBadWriter(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((/ 1 0))))`, scope).Eval(scope, nil)
	tt.Panic(t, func() { _ = slip.ReadString(`(send toot :report t)`, scope).Eval(scope, nil) })
}

func TestTestRunPanics(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((/ 3 2))))`, scope).Eval(scope, nil)
	tt.Panic(t, func() { _ = slip.ReadString(`(send toot :run t)`, scope).Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString(`(send toot :run :verbose)`, scope).Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString(`(send toot :run :bad t)`, scope).Eval(scope, nil) })
}

func TestTestInitPanic(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadString(`(make-instance 'test-flavor
                                            :name "toot"
                                            :parent (make-instance 'vanilla-flavor)
                                            :forms '(nil))`, scope).Eval(scope, nil)
	})
}
