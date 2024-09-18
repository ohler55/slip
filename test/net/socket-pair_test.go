// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSocketPairBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((pair (socket-pair :unix :stream nil))
                        (s0 (nth-value 0 pair))
                        (s1 (nth-value 1 pair)))
                  (send s0 :close)
                  (send s1 :close)
                  (list s0 s1))`,
		Expect: `/\(#<socket [0-9a-f]+> #<socket [0-9a-f]+>\)/`,
	}).Test(t)
}

func TestSocketPairNonblock(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((pair (socket-pair :unix :stream nil :nonblock t :cloexec t))
                        (s0 (nth-value 0 pair))
                        (s1 (nth-value 1 pair)))
                  (send s0 :close)
                  (send s1 :close)
                  (list s0 s1))`,
		Expect: `/\(#<socket [0-9a-f]+> #<socket [0-9a-f]+>\)/`,
	}).Test(t)
}

func TestSocketPairBadDomain(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-pair :bad-domain :stream nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSocketPairBadProtocol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(socket-pair :unix :stream :bad-protocol)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
