// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestBase64Decode(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-decode "c2FtcGxl")`,
		Expect: `"sample"`,
	}).Test(t)
}

func TestBase64DecodeError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(base64-decode "c2Ft##")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
