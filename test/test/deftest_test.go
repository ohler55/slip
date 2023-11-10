// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test_test

import (
	"bytes"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestDeftestBasic(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(setq toot (deftest "toot" nil (assert-nil (car '()))))`).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :describe out)`).Eval(scope, nil)
	tt.Equal(t, `/#<test-flavor [0-9a-f]+>, an instance of flavor test-flavor,
  has instance variable values:
    forms: \(\(assert-nil \(car '\(\)\)\)\)
    name: "toot"
    parent: nil
    result: nil
/`, out.String())

	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	out.Reset()
	_ = slip.ReadString(`(send toot :run :verbose t)`).Eval(scope, nil)
	tt.Equal(t, "toot: PASS\n", out.String())
}

func TestDeftestInSuite(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(setq top (defsuite "top" nil))`).Eval(scope, nil)
	_ = slip.ReadString(`(setq toot (deftest "toot" top (assert-nil (car '()))))`).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :describe out)`).Eval(scope, nil)
	tt.Equal(t, `/#<test-flavor [0-9a-f]+>, an instance of flavor test-flavor,
  has instance variable values:
    forms: \(\(assert-nil \(car '\(\)\)\)\)
    name: "toot"
    parent: #<suite-flavor [0-9a-f]+>
    result: nil
/`, out.String())

	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	out.Reset()
	_ = slip.ReadString(`(send top :run :verbose t)`).Eval(scope, nil)
	tt.Equal(t, `top:
  toot: PASS
-------------- top:
  passed:  1
  failed:  0
  skipped: 0
`, out.String())
}

func TestDeftestBadName(t *testing.T) {
	tt.Panic(t, func() { _ = slip.ReadString(`(deftest t nil)`).Eval(slip.NewScope(), nil) })
}

func TestDeftestBadParent(t *testing.T) {
	tt.Panic(t, func() { _ = slip.ReadString(`(deftest "bad" t)`).Eval(slip.NewScope(), nil) })
}
