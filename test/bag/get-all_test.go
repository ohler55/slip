// Copyright (c) 2026, Peter Ohler, All rights reserved.

package bag_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestBagGetAllStringPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((bag (make-bag "{a:[1 2 3]}")))
                   (bag-get-all bag "a[*]" :native))`,
		Expect: "(1 2 3)",
	}).Test(t)
}

func TestBagGetAllBagPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((bag (make-bag "{a:[1 2 3]}")))
                   (bag-get-all bag (make-bag-path "a[*]") :native))`,
		Expect: "(1 2 3)",
	}).Test(t)
}

func TestBagGetAllOptionBagList(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((bag (make-bag "{a:[1 2]}")))
                   (bag-get-all bag "a[*]" :bag-list))`,
		Expect: `/\(#<bag-flavor [0-9a-f]+> #<bag-flavor [0-9a-f]+>\)/`,
	}).Test(t)
}

func TestBagGetAllOptionBag(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((bag (make-bag "{a:[1 2]}")))
                   (send (bag-get-all bag "a[*]" :bag) :native))`,
		Expect: `(1 2)`,
	}).Test(t)
}

func TestBagGetAllNotBag(t *testing.T) {
	(&sliptest.Function{
		Source:    `(bag-get-all t "x")`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestBagGetAllBadPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((bag (make-bag "{a:[1 2 3]}")))
                   (bag-get-all bag t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestBagGetAllBadReturnType(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((bag (make-bag "{a:[1 2 3]}")))
                   (bag-get-all bag "a[*]" :quux))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestBagSendGetAll(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((bag (make-bag "{a:[1 2 3]}")))
                   (send bag :get-all "a[*]" :native))`,
		Expect: "(1 2 3)",
	}).Test(t)
}

func TestBagSendGetAllArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((bag (make-bag "{a:[1 2 3]}")))
                   (send bag :get-all "a[*]" :native t))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
