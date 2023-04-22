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

func TestBagModifyNilPath(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "[1 2 3]"))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-modify bag 'reverse)`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "[3 2 1]", pretty.SEN(obj.Any))
}

func TestBagModifyStringPath(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "{a:1 b:[1 2 3]}"))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-modify bag 'reverse "b")`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{a: 1 b: [3 2 1]}", pretty.SEN(obj.Any))
}

func TestBagModifyLambdaBag(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "{a:1 b:[1 2 3]}"))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-modify bag (lambda (b) (send b :set 0 "[1]")) "b" :as-bag t)`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{a: 1 b: [1 0 3]}", pretty.SEN(obj.Any))
}

func TestBagModifyLambdaBagObj(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "{a:1 b:[1 2 3]}"))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-modify bag (lambda (b) (send b :get "[1]")) "b" :as-bag t)`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{a: 1 b: 2}", pretty.SEN(obj.Any))
}

func TestBagModifyLambdaObjBag(t *testing.T) {
	scope := slip.NewScope()
	obj := slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "{a:1 b:[1 2 3]}"))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-modify bag (lambda (b) (make-instance 'bag-flavor :set (reverse b))) (make-bag-path "b"))`,
		Expect: "/#<bag-flavor [0-9a-f]+>/",
	}).Test(t)
	tt.Equal(t, "{a: 1 b: [3 2 1]}", pretty.SEN(obj.Any))
}

func TestBagModifyBadPath(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "{a:1 b:[1 2 3]}"))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-modify bag 'reverse t)`,
		Panics: true,
	}).Test(t)
}

func TestBagModifyNotBag(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-modify t 'reverse)`,
		Panics: true,
	}).Test(t)
}

func TestBagModifyBadKeyword(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(
		`(setq bag (make-instance 'bag-flavor :parse "{a:1 b:[1 2 3]}"))`).Eval(scope, nil).(*flavors.Instance)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-modify bag 'reverse nil t t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-modify bag 'reverse nil :as-not t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(bag-modify bag 'reverse nil :as-bag)`,
		Panics: true,
	}).Test(t)
}
