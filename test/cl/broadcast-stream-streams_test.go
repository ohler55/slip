// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestBroadcastStreamStreamsOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((ss1 (make-string-output-stream))
                        (ss2 (make-string-output-stream))
                        (bs (make-broadcast-stream ss1 ss2)))
                 (broadcast-stream-streams bs))`,
		Expect: "(#<STRING-STREAM> #<STRING-STREAM>)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(broadcast-stream-streams (make-broadcast-stream))`,
		Expect: "nil",
	}).Test(t)
}

func TestBroadcastStreamStreamsNotBS(t *testing.T) {
	(&sliptest.Function{
		Source:    `(broadcast-stream-streams t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
