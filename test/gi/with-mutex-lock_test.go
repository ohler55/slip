// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWithMutexLock(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((m (make-mutex))
                       (done (make-channel 1))
                       lst)

                   (run (with-mutex-lock m
                          (setq lst (add lst 'other))
                          (channel-push done t)
                          (channel-close done)))

                   (with-mutex-lock m
                     (sleep 0.1) ;; enough for the run to get started most of the time
                     (setq lst (add lst 'main)))
                   (channel-pop done)
                   lst)`,
		Expect: "(main other)",
	}).Test(t)
}

func TestWithMutexLockNotMutex(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-mutex-lock t nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
