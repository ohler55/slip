// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test_test

import (
	"bytes"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestAssertEqualFail(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadEvalString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-equal 3 (+ 2 2) "sample"))))`,
		scope)
	_ = slip.ReadEvalString(`(send toot :run :verbose t)`, scope)
	tt.Equal(t, `toot: FAIL
  expect: 3
  actual: 4
  sample

  (assert-equal 3 (+ 2 2) "sample")
`, out.String())
}

func TestAssertEqualFailStringLen(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", slip.True)
	_ = slip.ReadEvalString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-equal "abc" "ab" "sample"))))`,
		scope)
	_ = slip.ReadEvalString(`(send toot :run :verbose t)`, scope)
	tt.Equal(t, `/FAIL/`, out.String())
	tt.Equal(t, `/"abc"/`, out.String())
	tt.Equal(t, `/"ab"/`, out.String())

	out.Reset()
	_ = slip.ReadEvalString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-equal "ab" "abc" "sample"))))`,
		scope)
	_ = slip.ReadEvalString(`(send toot :run :verbose t)`, scope)
	tt.Equal(t, `/FAIL/`, out.String())
	tt.Equal(t, `/"abc"/`, out.String())
	tt.Equal(t, `/"ab"/`, out.String())
}

func TestAssertEqualMessageFail(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadEvalString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-equal 3 (+ 2 2) 'sample))))`,
		scope)
	_ = slip.ReadEvalString(`(send toot :run :verbose t)`, scope)
	tt.Equal(t, `toot: FAIL
  expect: 3
  actual: 4
  sample

  (assert-equal 3 (+ 2 2) 'sample)
`, out.String())
}

func TestAssertEqualPass(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadEvalString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-equal 3 (+ 1 2) 'sample))))`,
		scope)
	_ = slip.ReadEvalString(`(send toot :run :verbose t)`, scope)
	tt.Equal(t, `toot: PASS
`, out.String())
}

func TestAssertEqualStringFail(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadEvalString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-equal "abcdefg" "abcdxfg"))))`,
		scope)
	_ = slip.ReadEvalString(`(send toot :run :verbose t)`, scope)
	_ = slip.ReadEvalString(`(send toot :report)`, scope)

	expect := "toot: \x1b[1mFAIL\x1b[m\n" +
		"  \x1b[0;31m\x1b[mexpect: \"abcdefg\"\n" +
		"  actual: \"abcd\x1b[0;31mxfg\"\n" +
		"\x1b[m\n" +
		"  \x1b[0;31m(assert-equal \"abcdefg\" \"abcdxfg\")\x1b[m\n" +
		"toot: \x1b[1;31mFAIL\x1b[m\n"

	tt.Equal(t, expect, out.String())
}

func TestAssertEqualAnsi(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadEvalString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-equal 3 (+ 1 1)))))`, scope)
	_ = slip.ReadEvalString(`(send toot :run :verbose t)`, scope)

	expect := "toot: \x1b[1mFAIL\x1b[m\n" +
		"  \x1b[0;31m\x1b[mexpect: 3\n" +
		"  actual: \x1b[0;31m2\n" +
		"\x1b[m\n" +
		"  \x1b[0;31m(assert-equal 3 (+ 1 1))\x1b[m\n"

	tt.Equal(t, expect, out.String())
}
