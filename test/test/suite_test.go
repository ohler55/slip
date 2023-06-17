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

	_ = slip.ReadString(`(describe-method 'suite-flavor :run)`).Eval(scope, nil)
	tt.Equal(t, "/:run is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'suite-flavor :result)`).Eval(scope, nil)
	tt.Equal(t, "/:result is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'suite-flavor :report)`).Eval(scope, nil)
	tt.Equal(t, "/:report is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'suite-flavor :reset)`).Eval(scope, nil)
	tt.Equal(t, "/:reset is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'suite-flavor :init)`).Eval(scope, nil)
	tt.Equal(t, "/:init is a method of suite-flavor/", out.String())

	out.Reset()
	_ = slip.ReadString(`(describe-method 'suite-flavor :find)`).Eval(scope, nil)
	tt.Equal(t, "/:find is a method of suite-flavor/", out.String())
}

func TestSuiteRunBasic(t *testing.T) {
	scope := buildSuiteScope()
	var out bytes.Buffer
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(send sweet :run)`).Eval(scope, nil)
	result := slip.ReadString(`(send (send top :result) :write nil)`).Eval(scope, nil)
	tt.Equal(t, `{
  fail: 1
  pass: 1
  skip: 0
  subs: {sweet: {fail: 1 pass: 1 skip: 0 subs: {boom: fail toot: pass}}}
}`, string(result.(slip.String)))

	_ = slip.ReadString(`(send top :report out)`).Eval(scope, nil)
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

func TestSuiteRunVerbose(t *testing.T) {
	scope := buildSuiteScope()
	var out bytes.Buffer
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(send top :run :verbose t)`).Eval(scope, nil)
	tt.Equal(t, `top:
  sweet:
    toot: PASS
    boom: FAIL
      runtime error: integer divide by zero
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
	_ = slip.ReadString(`(send top :report)`).Eval(scope, nil)
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
	found := slip.ReadString(`(send top :find 'sweet 'boom)`).Eval(scope, nil)
	tt.SameType(t, &flavors.Instance{}, found)
	scope.Let(slip.Symbol("found"), found)
	name := slip.ReadString(`(send found :name)`).Eval(scope, nil)
	tt.Equal(t, "boom", string(name.(slip.String)))

	found = slip.ReadString(`(send top :find 'sweet "nothing")`).Eval(scope, nil)
	tt.Nil(t, found)

	found = slip.ReadString(`(send top :find)`).Eval(scope, nil)
	tt.Nil(t, found)

	found = slip.ReadString(`(send top :find t)`).Eval(scope, nil)
	tt.Nil(t, found)

	found = slip.ReadString(`(send top :find '())`).Eval(scope, nil)
	tt.SameType(t, &flavors.Instance{}, found)
	scope.Let(slip.Symbol("found"), found)
	name = slip.ReadString(`(send found :name)`).Eval(scope, nil)
	tt.Equal(t, "top", string(name.(slip.String)))
}

func TestSuiteRunFilter(t *testing.T) {
	scope := buildSuiteScope()
	var out bytes.Buffer
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(send top :run :filter '(sweet toot))`).Eval(scope, nil)
	result := slip.ReadString(`(send (send top :result) :write nil)`).Eval(scope, nil)
	tt.Equal(t, `{
  fail: 0
  pass: 1
  skip: 1
  subs: {sweet: {fail: 0 pass: 1 skip: 1 subs: {boom: null toot: pass}}}
}`, string(result.(slip.String)))
}

func TestSuiteReportBadWriter(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq sweet (make-instance 'suite-flavor :name "sweet"))`).Eval(scope, nil)
	tt.Panic(t, func() { _ = slip.ReadString(`(send sweet :report t)`).Eval(scope, nil) })
}

func buildSuiteScope() *slip.Scope {
	scope := slip.NewScope()
	_ = slip.ReadString(`
(setq top (make-instance 'suite-flavor :name "top")
      sweet (make-instance 'suite-flavor :name "sweet" :parent top)
      toot (make-instance 'test-flavor :name "toot" :parent sweet :forms '((+ 1 (/ 2 3))))
      boom (make-instance 'test-flavor :name "boom" :parent sweet :forms '((/ 1 0))))
`).Eval(scope, nil)
	return scope
}
