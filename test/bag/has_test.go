// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestBagHasStringPath(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope).(*flavors.Instance)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-has bag "a")`,
		Expect: "t",
	}).Test(t)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-has bag "b")`,
		Expect: "nil",
	}).Test(t)
}

func TestBagHasBagPath(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-has bag (make-bag-path "a"))`,
		Expect: "t",
	}).Test(t)
}

func TestBagHasArgCount(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-has bag)`,
		Panics: true,
	}).Test(t)
}

func TestBagHasBadPath(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-has bag t)`,
		Panics: true,
	}).Test(t)
}

func TestBagHasBadBag(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-has t "a")`,
		Panics: true,
	}).Test(t)
}
