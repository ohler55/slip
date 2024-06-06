// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSignalWait(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((wake (make-channel 1)))
                  (run ((lambda (chan)
                         (channel-push chan t)
                         (signal-wait sigcont)
                         (channel-push chan t)) wake))
                  (channel-pop wake)
                  (sleep 0.1)
                  (send-signal (process-id) sigcont)
                  (channel-pop wake))`,
		Expect: "t",
	}).Test(t)
}

func TestSignalWaitBadSignal(t *testing.T) {
	(&sliptest.Function{
		Source:    `(signal-wait t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
