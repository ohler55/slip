// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test_test

import (
	"bytes"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func TestSuiteDocs(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)

	_ = slip.ReadString(`(describe-method 'suite-flavor :run)`, scope).Eval(scope, nil)
	tt.Equal(t, "/:run is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'suite-flavor :result)`, scope).Eval(scope, nil)
	tt.Equal(t, "/:result is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'suite-flavor :report)`, scope).Eval(scope, nil)
	tt.Equal(t, "/:report is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'suite-flavor :reset)`, scope).Eval(scope, nil)
	tt.Equal(t, "/:reset is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'suite-flavor :init)`, scope).Eval(scope, nil)
	tt.Equal(t, "/:init is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'suite-flavor :find)`, scope).Eval(scope, nil)
	tt.Equal(t, "/:find is a method of suite-flavor/", out.String())
}

func TestSuiteRunBasic(t *testing.T) {
	scope := buildSuiteScope()
	var stdout bytes.Buffer
	var out bytes.Buffer
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &stdout})
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(send sweet :run)`, scope).Eval(scope, nil)
	result := slip.ReadString(`(send (send top :result) :write nil)`, scope).Eval(scope, nil)
	tt.Equal(t, `{
  fail: 1
  pass: 1
  skip: 0
  subs: {sweet: {fail: 1 pass: 1 skip: 0 subs: {boom: fail toot: pass}}}
}`, string(result.(slip.String)))

	_ = slip.ReadString(`(send top :report out)`, scope).Eval(scope, nil)
	tt.Equal(t, `top:
  sweet:
    toot: PASS
    boom: FAIL
  -------------- sweet:
    passed:  1
    failed:  1
    skipped: 0
-------------- top:
  passed:  1
  failed:  1
  skipped: 0
`, out.String())

	tt.Equal(t, `    boom: FAIL
      divide by zero
`, stdout.String())
}

func TestSuiteRunVerbose(t *testing.T) {
	scope := buildSuiteScope()
	var out bytes.Buffer
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(send top :run :verbose t)`, scope).Eval(scope, nil)
	tt.Equal(t, `top:
  sweet:
    toot: PASS
    boom: FAIL
      divide by zero
      (/ 1 0)
  -------------- sweet:
    passed:  1
    failed:  1
    skipped: 0
-------------- top:
  passed:  1
  failed:  1
  skipped: 0
`, out.String())

	out.Reset()
	_ = slip.ReadString(`(send top :report)`, scope).Eval(scope, nil)
	tt.Equal(t, `top:
  sweet:
    toot: PASS
    boom: FAIL
  -------------- sweet:
    passed:  1
    failed:  1
    skipped: 0
-------------- top:
  passed:  1
  failed:  1
  skipped: 0
`, out.String())
}

func TestSuiteFind(t *testing.T) {
	scope := buildSuiteScope()
	found := slip.ReadString(`(send top :find 'sweet 'boom)`, scope).Eval(scope, nil)
	tt.SameType(t, &flavors.Instance{}, found)
	scope.Let(slip.Symbol("found"), found)
	name := slip.ReadString(`(send found :name)`, scope).Eval(scope, nil)
	tt.Equal(t, "boom", string(name.(slip.String)))

	found = slip.ReadString(`(send top :find 'sweet "nothing")`, scope).Eval(scope, nil)
	tt.Nil(t, found)

	found = slip.ReadString(`(send top :find)`, scope).Eval(scope, nil)
	tt.Nil(t, found)

	found = slip.ReadString(`(send top :find t)`, scope).Eval(scope, nil)
	tt.Nil(t, found)

	found = slip.ReadString(`(send top :find '())`, scope).Eval(scope, nil)
	tt.SameType(t, &flavors.Instance{}, found)
	scope.Let(slip.Symbol("found"), found)
	name = slip.ReadString(`(send found :name)`, scope).Eval(scope, nil)
	tt.Equal(t, "top", string(name.(slip.String)))
}

func TestSuiteRunFilter(t *testing.T) {
	scope := buildSuiteScope()
	var out bytes.Buffer
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(send top :run :filter '(sweet toot))`, scope).Eval(scope, nil)
	result := slip.ReadString(`(send (send top :result) :write nil)`, scope).Eval(scope, nil)
	tt.Equal(t, `{
  fail: 0
  pass: 1
  skip: 1
  subs: {sweet: {fail: 0 pass: 1 skip: 1 subs: {boom: null toot: pass}}}
}`, string(result.(slip.String)))
}

func TestSuiteReportBadWriter(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq sweet (make-instance 'suite-flavor :name "sweet"))`, scope).Eval(scope, nil)
	tt.Panic(t, func() { _ = slip.ReadString(`(send sweet :report t)`, scope).Eval(scope, nil) })
}

func buildSuiteScope() *slip.Scope {
	scope := slip.NewScope()
	_ = slip.ReadString(`
(setq top (make-instance 'suite-flavor :name "top")
      sweet (make-instance 'suite-flavor :name "sweet" :parent top)
      toot (make-instance 'test-flavor :name "toot" :parent sweet :forms '((+ 1 (/ 2 3))))
      boom (make-instance 'test-flavor :name "boom" :parent sweet :forms '((/ 1 0))))
`, scope).Eval(scope, nil)
	return scope
}

func TestSuiteRunSetup(t *testing.T) {
	scope := buildSuiteScope()
	var out bytes.Buffer
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(send sweet :set-setup (lambda () (setq suite-setup t)))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send sweet :set-teardown (lambda () (setq suite-teardown t)))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send top :run)`, scope).Eval(scope, nil)

	tt.NotNil(t, scope.Get("suite-setup"))
	tt.NotNil(t, scope.Get("suite-teardown"))
}
