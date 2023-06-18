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
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-equal 3 (+ 2 2)))))`).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :verbose t)`).Eval(scope, nil)
	tt.Equal(t, `toot: FAIL
  expect: 3
  actual: 4

  (assert-equal 3 (+ 2 2))
`, out.String())
}

func TestAssertEqualStringFail(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-equal "abcdefg" "abcdxfg"))))`).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :verbose t)`).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :report)`).Eval(scope, nil)

	expect := "toot: \x1b[1mFAIL\x1b[m\n" +
		"  \x1b[0;31m\x1b[mexpect: abcdefg\n" +
		"  actual: abcd\x1b[0;31mxfg\n" +
		"\x1b[m\n" +
		"  \x1b[0;31m(assert-equal \"abcdefg\" \"abcdxfg\")\x1b[m\n" +
		"toot: \x1b[1;31mFAIL\x1b[m\n"

	tt.Equal(t, expect, out.String())
}
