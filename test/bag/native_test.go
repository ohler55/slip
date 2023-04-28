// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestBagNativeOk(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-native bag)`,
		Expect: `(("a" . 7))`,
	}).Test(t)
}

func TestBagNativeNotBag(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-native t)`,
		Panics: true,
	}).Test(t)
}

func TestBagNativeArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-native)`,
		Panics: true,
	}).Test(t)
}
