// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestWithStandardIoSyntaxOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-output-to-string (s)
                  (let ((*print-base* 16))
                    (princ 20 s) (princ " " s)
                    (with-standard-io-syntax (princ 20 s))))`,
		Expect: `"14 20"`,
	}).Test(t)
}

func TestWithStandardIoSyntaxReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(block nil (with-standard-io-syntax (+ 1 2) (return 7) 8))`,
		Expect: "7",
	}).Test(t)
}
