// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestConcatenatedStreamStreamsOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((ss1 (make-string-input-stream "abc"))
                        (ss2 (make-string-input-stream "def"))
                        (cs (make-concatenated-stream ss1 ss2)))
                 (concatenated-stream-streams cs))`,
		Expect: "(#<STRING-STREAM> #<STRING-STREAM>)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(concatenated-stream-streams (make-concatenated-stream))`,
		Expect: "nil",
	}).Test(t)
}

func TestBoradcastStreamStreamsNotCS(t *testing.T) {
	(&sliptest.Function{
		Source:    `(concatenated-stream-streams t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
