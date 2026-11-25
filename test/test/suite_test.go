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

	_ = slip.ReadEvalString(`(describe-method 'suite-flavor :run)`, scope)
	tt.Equal(t, "/:run is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadEvalString(`(describe-method 'suite-flavor :result)`, scope)
	tt.Equal(t, "/:result is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadEvalString(`(describe-method 'suite-flavor :report)`, scope)
	tt.Equal(t, "/:report is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadEvalString(`(describe-method 'suite-flavor :reset)`, scope)
	tt.Equal(t, "/:reset is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadEvalString(`(describe-method 'suite-flavor :init)`, scope)
	tt.Equal(t, "/:init is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadEvalString(`(describe-method 'suite-flavor :find)`, scope)
	tt.Equal(t, "/:find is a method of suite-flavor/", out.String())
}

func TestSuiteRunBasic(t *testing.T) {
	scope := buildSuiteScope()
	var stdout bytes.Buffer
	var out bytes.Buffer
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &stdout})
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadEvalString(`(send sweet :run)`, scope)
	result := slip.ReadEvalString(`(send (send top :result) :write nil)`, scope)
	tt.Equal(t, `{
  fail: 1
  pass: 1
  skip: 0
  subs: {sweet: {fail: 1 pass: 1 skip: 0 subs: {boom: fail toot: pass}}}
}`, string(result.(slip.String)))

	_ = slip.ReadEvalString(`(send top :report out)`, scope)
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
	_ = slip.ReadEvalString(`(send top :run :verbose t)`, scope)
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
	_ = slip.ReadEvalString(`(send top :report)`, scope)
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
	found := slip.ReadEvalString(`(send top :find 'sweet 'boom)`, scope)
	tt.SameType(t, &flavors.Instance{}, found)
	scope.Let(slip.Symbol("found"), found)
	name := slip.ReadEvalString(`(send found :name)`, scope)
	tt.Equal(t, "boom", string(name.(slip.String)))

	found = slip.ReadEvalString(`(send top :find 'sweet "nothing")`, scope)
	tt.Nil(t, found)

	found = slip.ReadEvalString(`(send top :find)`, scope)
	tt.Nil(t, found)

	found = slip.ReadEvalString(`(send top :find t)`, scope)
	tt.Nil(t, found)

	found = slip.ReadEvalString(`(send top :find '())`, scope)
	tt.SameType(t, &flavors.Instance{}, found)
	scope.Let(slip.Symbol("found"), found)
	name = slip.ReadEvalString(`(send found :name)`, scope)
	tt.Equal(t, "top", string(name.(slip.String)))
}

func TestSuiteRunFilter(t *testing.T) {
	scope := buildSuiteScope()
	var out bytes.Buffer
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadEvalString(`(send top :run :filter '(sweet toot))`, scope)
	result := slip.ReadEvalString(`(send (send top :result) :write nil)`, scope)
	tt.Equal(t, `{
  fail: 0
  pass: 1
  skip: 1
  subs: {sweet: {fail: 0 pass: 1 skip: 1 subs: {boom: null toot: pass}}}
}`, string(result.(slip.String)))
}

func TestSuiteRunFilterRegexp(t *testing.T) {
	scope := buildSuiteScope()
	var out bytes.Buffer
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadEvalString(`(send top :run :filter "/swe*t/./to*t/")`, scope)
	result := slip.ReadEvalString(`(send (send top :result) :write nil)`, scope)
	tt.Equal(t, `{
  fail: 0
  pass: 1
  skip: 1
  subs: {sweet: {fail: 0 pass: 1 skip: 1 subs: {boom: null toot: pass}}}
}`, string(result.(slip.String)))
}

func TestSuiteReportBadWriter(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadEvalString(`(setq sweet (make-instance 'suite-flavor :name "sweet"))`, scope)
	tt.Panic(t, func() { _ = slip.ReadEvalString(`(send sweet :report t)`, scope) })
}

func buildSuiteScope() *slip.Scope {
	scope := slip.NewScope()
	_ = slip.ReadEvalString(`
(setq top (make-instance 'suite-flavor :name "top")
      sweet (make-instance 'suite-flavor :name "sweet" :parent top)
      toot (make-instance 'test-flavor :name "toot" :parent sweet :forms '((+ 1 (/ 2 3))))
      boom (make-instance 'test-flavor :name "boom" :parent sweet :forms '((/ 1 0))))
`, scope)
	return scope
}

func TestSuiteRunSetup(t *testing.T) {
	scope := buildSuiteScope()
	var out bytes.Buffer
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadEvalString(`(send sweet :set-setup (lambda () (setq suite-setup t)))`, scope)
	_ = slip.ReadEvalString(`(send sweet :set-teardown (lambda () (setq suite-teardown t)))`, scope)
	_ = slip.ReadEvalString(`(send top :run)`, scope)

	tt.NotNil(t, scope.Get("suite-setup"))
	tt.NotNil(t, scope.Get("suite-teardown"))
}
