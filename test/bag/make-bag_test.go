// Copyright (c) 2024, Peter Ohler, All rights reserved.

package bag_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMakeBagParse(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-bag "{a:7}") :write)`,
		Expect: `"{a: 7}"`,
	}).Test(t)
}

func TestMakeBagParseOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-bag (coerce "{a:7}" 'octets)) :write)`,
		Expect: `"{a: 7}"`,
	}).Test(t)
}

func TestMakeBagSet(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-bag '((a . 7))) :write)`,
		Expect: `"{a: 7}"`,
	}).Test(t)
}
