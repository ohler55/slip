// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag_test

import (
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestBagSetStringPath(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-set bag 3 "a")`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{a: 3}", pretty.SEN(obj.Any))
}

func TestBagSetNoPath(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-set bag 3)`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "3", pretty.SEN(obj.Any))
}

func TestBagSetArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-set (make-instance 'bag-flavor))`,
		Panics: true,
	}).Test(t)
}

func TestBagSetNotBag(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-set (make-instance 'vanilla-flavor) 3)`,
		Panics: true,
	}).Test(t)
}

func TestBagSetBadPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-set (make-instance 'bag-flavor) 3 t)`,
		Panics: true,
	}).Test(t)
}
