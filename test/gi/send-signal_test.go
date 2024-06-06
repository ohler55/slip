// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSendSignalBadPid(t *testing.T) {
	(&sliptest.Function{
		Source:    `(send-signal t sigcont)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSendSignalBadSignal(t *testing.T) {
	(&sliptest.Function{
		Source:    `(send-signal 1234 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSendSignalNoProcess(t *testing.T) {
	(&sliptest.Function{
		Source:    `(send-signal -1 -1)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
