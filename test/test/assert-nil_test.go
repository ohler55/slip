// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test_test

import (
	"bytes"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestAssertNilFail(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-nil t "sample"))))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :verbose t)`, scope).Eval(scope, nil)
	tt.Equal(t, `toot: FAIL
  expect: nil
  actual: t
  sample

  (assert-nil t "sample")
`, out.String())
}

func TestAssertNilMessageFail(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-nil t 'sample))))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :verbose t)`, scope).Eval(scope, nil)
	tt.Equal(t, `toot: FAIL
  expect: nil
  actual: t
  sample

  (assert-nil t 'sample)
`, out.String())
}

func TestAssertNilPass(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-nil (car '()) 'sample))))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :verbose t)`, scope).Eval(scope, nil)
	tt.Equal(t, `toot: PASS
`, out.String())
}

func TestAssertNilAnsi(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-nil t))))`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :verbose t)`, scope).Eval(scope, nil)

	expect := "toot: \x1b[1mFAIL\x1b[m\n" +
		"  \x1b[0;31m\x1b[mexpect: nil\n" +
		"  actual: \x1b[0;31mt\n" +
		"\x1b[m\n" +
		"  \x1b[0;31m(assert-nil t)\x1b[m\n"

	tt.Equal(t, expect, out.String())
}
