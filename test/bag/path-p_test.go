// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestBagPathpOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-path-p (make-bag-path "a.b"))`,
		Expect: `t`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(bag-path-p "x")`,
		Expect: `nil`,
	}).Test(t)
}
func TestBagPathpArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-path-p)`,
		Panics: true,
	}).Test(t)
}
