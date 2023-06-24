// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test_test

import (
	"bytes"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestAssertPanicFail(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-panic (lambda () nil) "sample"))
                                    ))`).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :verbose t)`).Eval(scope, nil)
	tt.Equal(t, `toot: FAIL
  expect: panic
  actual: did not panic
  sample

  (assert-panic (lambda () nil) "sample")
`, out.String())
}

func TestAssertPanicMessageFail(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-panic (lambda () nil) 'sample))
                                    ))`).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :verbose t)`).Eval(scope, nil)
	tt.Equal(t, `toot: FAIL
  expect: panic
  actual: did not panic
  sample

  (assert-panic (lambda () nil) 'sample)
`, out.String())
}

func TestAssertPanicPass(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-panic '(car 7) 'sample))
                                    ))`).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :verbose t)`).Eval(scope, nil)
	tt.Equal(t, `toot: PASS
`, out.String())
}

func TestAssertPanicAnsi(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-panic t))))`).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :verbose t)`).Eval(scope, nil)

	expect := "toot: \x1b[1mFAIL\x1b[m\n" +
		"  \x1b[0;31m\x1b[mexpect: panic\n" +
		"  actual: \x1b[0;31mdid not panic\n" +
		"\x1b[m\n" +
		"  \x1b[0;31m(assert-panic t)\x1b[m\n"

	tt.Equal(t, expect, out.String())
}
