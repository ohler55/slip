// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestStreamErrorStreamExact(t *testing.T) {
	(&sliptest.Function{
		Source: `(stream-error-stream (make-condition 'stream-error :stream *standard-output*))`,
		Expect: `/#<FILE-STREAM .*stdout .*>/`,
	}).Test(t)
}

func TestStreamErrorStreamNotStreamError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(stream-error-stream (make-condition 'error :stream *standard-output*))`,
		PanicType: slip.Symbol("unbound-slot"),
	}).Test(t)
}
