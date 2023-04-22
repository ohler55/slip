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

func TestBagGetStringPath(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope, nil).(*flavors.Instance)
	st := sliptest.Function{
		Scope:  scope,
		Source: `(bag-get bag "a" t)`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}
	st.Test(t)
	bg := st.Result.(*flavors.Instance)
	tt.Equal(t, "7", pretty.SEN(bg.Any))
}

func TestBagGetBagPath(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-get bag (make-bag-path "a") nil)`,
		Expect: "7",
	}).Test(t)
}

func TestBagGetBagNoPath(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope, nil).(*flavors.Instance)
	st := sliptest.Function{
		Scope:  scope,
		Source: `(bag-get bag nil t)`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}
	st.Test(t)
	bg := st.Result.(*flavors.Instance)
	tt.Equal(t, "{a: 7}", pretty.SEN(bg.Any))

	st = sliptest.Function{
		Scope:  scope,
		Source: `(bag-get bag)`,
		Expect: `(("a" . 7))`,
	}
	st.Test(t)
}

func TestBagGetStringLisp(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-get bag nil)`,
		Expect: `(("a" . 7))`,
	}).Test(t)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-get bag "b")`,
		Expect: "nil",
	}).Test(t)
}

func TestBagGetArgCount(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-get)`,
		Panics: true,
	}).Test(t)
}

func TestBagGetNotBag(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-get t)`,
		Panics: true,
	}).Test(t)
}

func TestBagGetBadPath(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(setq bag (make-instance 'bag-flavor))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-get bag t)`,
		Panics: true,
	}).Test(t)
}
