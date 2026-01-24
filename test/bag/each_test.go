// Copyright (c) 2026, Peter Ohler, All rights reserved.

package bag_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestBagEachFile(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (result)
                   (each-bag "testdata/multi.json"
                     (lambda (b)
                       (setq result (add result (send b :write nil)))))
                   result)`,
		Expect: `("{a: 1}" "{b: 2}")`,
	}).Test(t)
}

func TestBagEachStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (result)
                   (with-open-file (f "testdata/multi.json" :direction :input)
                     (each-bag f
                       (lambda (b)
                         (setq result (add result (send b :write nil))))))
                   result)`,
		Expect: `("{a: 1}" "{b: 2}")`,
	}).Test(t)
}

func TestBagEachNotFunction(t *testing.T) {
	(&sliptest.Function{
		Source:    `(each-bag "testdata/sample.json" t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestBagEachBadInput(t *testing.T) {
	(&sliptest.Function{
		Source:    `(each-bag t (lambda (b) nil))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
