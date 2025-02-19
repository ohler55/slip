// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestInteractiveStreamPOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(interactive-stream-p *standard-input*)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(interactive-stream-p (make-string-input-stream "x"))`,
		Expect: "nil",
	}).Test(t)
}

func TestInteractiveStreamPLet(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*standard-input* (make-string-input-stream "x")))
                  (interactive-stream-p *standard-input*))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let* ((ss (make-string-input-stream "x"))
                        (*standard-input* ss))
                  (interactive-stream-p ss))`,
		Expect: "t",
	}).Test(t)
}

func TestInteractiveStreamPNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(interactive-stream-p t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
