// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRangeChannel(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((chan (make-channel 10))
                       result)
                  (channel-push chan 1)
                  (channel-push chan 2)
                  (channel-close chan)
                  (range (lambda (x) (addf result x)) chan)
                  result)`,
		Expect: "(1 2)",
	}).Test(t)
}

func TestRangeList(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (result)
                  (range (lambda (x) (addf result x)) '(1 2))
                  result)`,
		Expect: "(1 2)",
	}).Test(t)
}

func TestRangeVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (result)
                  (range (lambda (x) (addf result x)) #(1 2))
                  result)`,
		Expect: "(1 2)",
	}).Test(t)
}

func TestRangeOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (result)
                  (range (lambda (x) (addf result x)) (coerce "abc" 'octets))
                  result)`,
		Expect: "(97 98 99)",
	}).Test(t)
}

func TestRangeNotCollection(t *testing.T) {
	(&sliptest.Function{
		Source:    `(range (lambda (x) (addf result x)) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
