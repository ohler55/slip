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
	obj := slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`, scope).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-set bag 3 "a")`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{a: 3}", pretty.SEN(obj.Any))
}

func TestBagSetPath(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(`(setq
                             path (make-bag-path "$.a")
                             bag (make-instance 'bag-flavor :parse "{a:7}"))
`, scope).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-set bag 3 path)`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{a: 3}", pretty.SEN(obj.Any))
}

func TestBagSetNoPath(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`, scope).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-set bag 3)`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "3", pretty.SEN(obj.Any))
}

func TestBagSetSymbol(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "7"))`, scope).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-set bag 'abc)`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "abc", pretty.SEN(obj.Any))

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-set bag :false)`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "false", pretty.SEN(obj.Any))
}

func TestBagSetList(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "7"))`, scope).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-set bag '(a b c))`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "[a b c]", pretty.SEN(obj.Any))

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-set bag '())`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "null", pretty.SEN(obj.Any))

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-set bag '(nil))`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "[null]", pretty.SEN(obj.Any))
}

func TestBagSetMap(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "7"))`, scope).Eval(scope, nil).(*flavors.Instance)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-set bag '((a . 1)))`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{a: 1}", pretty.SEN(obj.Any))

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-set bag '(("a" . 1)))`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{a: 1}", pretty.SEN(obj.Any))

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-set bag '((t . 1)))`,
		Panics: true,
	}).Test(t)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-set bag '((a . 1) t))`,
		Panics: true,
	}).Test(t)
}

func TestBagSetBag(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(`(setq
                             value (make-instance 'bag-flavor :set 3)
                             bag (make-instance 'bag-flavor :parse "7"))`, scope).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-set bag value)`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "3", pretty.SEN(obj.Any))

	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-set bag (make-instance 'vanilla-flavor))`,
		Panics: true,
	}).Test(t)
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
