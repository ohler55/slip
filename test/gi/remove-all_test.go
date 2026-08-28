// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRemoveAllOk(t *testing.T) {
	_ = os.MkdirAll("testdata/remove", 0755)
	_ = os.WriteFile("testdata/remove/remove.bak", []byte("something"), 0666)
	(&sliptest.Function{
		Source: `(remove-all "testdata/remove")`,
		Expect: "nil",
	}).Test(t)
	_, err := os.Stat("testdata/remove")
	tt.NotNil(t, err)
}

func TestRemoveAllNotString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(remove-all t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
