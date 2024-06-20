// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReadPushOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((chan (make-channel 2)))
                  (read-push (make-string-input-stream "(1 2 3)") chan)
                  (channel-pop chan))`,
		Expect: "(1 2 3)",
	}).Test(t)
}

func TestReadPushNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(read-push t (make-channel 1))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestReadPushNotChannel(t *testing.T) {
	(&sliptest.Function{
		Source:    `(read-push (make-string-input-stream "(1 2 3)") t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
