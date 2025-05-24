// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestUnzipBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce (nth-value 0 (unzip (zip "some data" 5))) 'string)`,
		Expect: `"some data"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(nth-value 1 (unzip (zip "some data" 5)))`,
		Expect: `nil`,
	}).Test(t)
}

func TestUnzipHeaders(t *testing.T) {
	(&sliptest.Function{
		Source: `(nth-value 1 (unzip (zip "some data" 5
                                          :name "namai"
                                          :comment "test"
                                          :extra "xxx"
                                          :os 7)))`,
		Array:  true,
		Expect: `(:comment "test" :extra #(120 120 120) :name "namai" :os 7)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(nth-value 1 (unzip (zip "some data" 5 :mod-time @2025-05-23T12:13:14Z)))`,
		Expect: `(:mod-time @2025-05-23T12:13:14Z)`,
	}).Test(t)
}

func TestUnzipError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(unzip "not a zip")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
