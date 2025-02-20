// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestStreamElementTypeOkay(t *testing.T) {
	(&sliptest.Function{
		Source: `(stream-element-type (make-string-output-stream))`,
		Expect: "octet",
	}).Test(t)
}

func TestStreamElementTypeNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(stream-element-type t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
