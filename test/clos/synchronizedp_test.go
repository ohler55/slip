// Copyright (c) 2026, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSynchronizedp(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((inst (make-instance 'vanilla-flavor))
                        (result (list (synchronizedp inst))))
                   (set-synchronized inst t)
                   (setq result (add result (synchronizedp inst)))
                   (set-synchronized inst nil)
                   (setq result (add result (synchronizedp inst)))
                   result)`,
		Expect: "(nil t nil)",
	}).Test(t)
}

func TestSynchronizedpNotInstance(t *testing.T) {
	(&sliptest.Function{
		Source:    `(synchronizedp t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
