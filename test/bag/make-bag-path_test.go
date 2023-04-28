// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMakeBagPathOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-bag-path "a.b.c")`,
		Expect: "#<bag-path a.b.c>",
	}).Test(t)
}

func TestMakeBagPathFromList(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-bag-path '(a "b" 3 nil))`,
		Expect: "#<bag-path a.b[3].*>",
	}).Test(t)
}

func TestMakeBagPathArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-bag-path)`,
		Panics: true,
	}).Test(t)
}

func TestMakeBagPathNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-bag-path t)`,
		Panics: true,
	}).Test(t)
}

func TestMakeBagPathBadListPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-bag-path '(t))`,
		Panics: true,
	}).Test(t)
}
