// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestStreamExternalFormatOkay(t *testing.T) {
	(&sliptest.Function{
		Source: `(stream-external-format (make-string-output-stream))`,
		Expect: ":default",
	}).Test(t)
}

func TestStreamExternalFormatNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(stream-external-format t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
