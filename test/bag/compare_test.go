// Copyright (c) 2023, Peter Ohler, All rights reserved.

package bag_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestBagCompareNoIgnores(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-compare
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 2 3]}")
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 0 3]}"))`,
		Expect: `("b" 1)`,
	}).Test(t)
}

func TestBagCompareIgnores(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-compare
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 2 3]}")
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 0 3]}")
                  '("b[1]"))`,
		Expect: `nil`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(bag-compare
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 2 3]}")
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 0 3]}")
                  '((b 1)))`,
		Expect: `nil`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(bag-compare
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 2 3]}")
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 0 3]}")
                  '(("b" 1)))`,
		Expect: `nil`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(bag-compare
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 2 3]}")
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 0 3]}")
                  '((nil 1)))`,
		Expect: `nil`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(bag-compare
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 2 3]}")
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 0 3]}")
                  (list (make-bag-path "$.*[1]")))`,
		Expect: `nil`,
	}).Test(t)
}

func TestBagCompareOtherNotBag(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-compare (make-instance 'bag-flavor :parse "{a:1 b:[1 2 3]}") t)`,
		Expect: `(nil)`,
	}).Test(t)
}

func TestBagCompareNotBag(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-compare t nil)`,
		Panics: true,
	}).Test(t)
}

func TestBagCompareBadIgnores(t *testing.T) {
	(&sliptest.Function{
		Source: `(bag-compare
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 2 3]}")
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 0 3]}")
                  t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(bag-compare
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 2 3]}")
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 0 3]}")
                  '(t))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(bag-compare
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 2 3]}")
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 0 3]}")
                  '((t)))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(bag-compare
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 2 3]}")
                  (make-instance 'bag-flavor :parse "{a:1 b:[1 0 3]}")
                  '("..[1]"))`,
		Panics: true,
	}).Test(t)
}
