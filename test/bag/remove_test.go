// Copyright (c) 2023, Peter Ohler, All rights reserved.

package bag_test

import (
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestBagRemoveStringPath(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "{a:7 b:8}"))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-remove bag "a")`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{b: 8}", pretty.SEN(obj.Any))
}

func TestBagRemovePath(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "{a:7 b:8}"))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-remove bag (make-bag-path "$.a"))`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{b: 8}", pretty.SEN(obj.Any))
}

func TestBagRemoveNoPath(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "{a:7 b:8}"))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-remove bag)`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "null", pretty.SEN(obj.Any))
}

func TestBagRemoveBadPath(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "{a:7 b:8}"))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-remove bag t)`,
		Panics: true,
	}).Test(t)
}

func TestBagRemoveNotBag(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-remove t)`,
		Panics: true,
	}).Test(t)
}
