// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestBase64Encode(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode "sample")`,
		Expect: `"c2FtcGxl"`,
	}).Test(t)
}
