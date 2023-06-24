// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test_test

import (
	"bytes"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestAssertMatchFail(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-match "^[0-9a-f]+$" "abcx123" "sample"))
                                     ))`).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :verbose t)`).Eval(scope, nil)
	tt.Equal(t, `toot: FAIL
  expect: /^[0-9a-f]+$/
  actual: "abcx123"
  sample

  (assert-match "^[0-9a-f]+$" "abcx123" "sample")
`, out.String())
}

func TestAssertMatchPass(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-match "^[0-9a-f]+$" 'abc123 "sample"))
                                     ))`).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :verbose t)`).Eval(scope, nil)
	tt.Equal(t, `toot: PASS
`, out.String())
}

func TestAssertMatchNumberPass(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-match "^[0-9a-f]+$" 123 'sample))
                                     ))`).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :verbose t)`).Eval(scope, nil)
	tt.Equal(t, `toot: PASS
`, out.String())
}

func TestAssertMatchNumberFail(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-match "^[0-9a-f]+$" 12.3 'sample))
                                     ))`).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :verbose t)`).Eval(scope, nil)
	tt.Equal(t, `toot: FAIL
  expect: /^[0-9a-f]+$/
  actual: 12.3
  sample

  (assert-match "^[0-9a-f]+$" 12.3 'sample)
`, out.String())
}

func TestAssertMatchAnsi(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-match "ab.." "abc"))))`).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run :verbose t)`).Eval(scope, nil)

	expect := "toot: \x1b[1mFAIL\x1b[m\n" +
		"  \x1b[0;31m\x1b[mexpect: /ab../\n" +
		"  actual: \x1b[0;31m\"abc\"\n" +
		"\x1b[m\n" +
		"  \x1b[0;31m(assert-match \"ab..\" \"abc\")\x1b[m\n"

	tt.Equal(t, expect, out.String())
}

func TestAssertNotReqexp(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	_ = slip.ReadString(`(setq toot (make-instance 'test-flavor
                                                   :name "toot"
                                                   :forms '((assert-match 3 3))))`).Eval(scope, nil)
	_ = slip.ReadString(`(send toot :run)`).Eval(scope, nil)
	tt.Equal(t, `toot: FAIL
  regexp must be a string not 3, a fixnum.
`, out.String())
}
