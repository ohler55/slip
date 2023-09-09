// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestClassNameFound(t *testing.T) {
	(&sliptest.Function{
		Source: `(class-name vanilla-flavor)`,
		Expect: "vanilla-flavor",
	}).Test(t)
	(&sliptest.Function{
		Source: `(class-name 'vanilla-flavor)`,
		Expect: "vanilla-flavor",
	}).Test(t)
}

func TestClassNameArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(class-name)`,
		Panics: true,
	}).Test(t)
}

func TestClassNameNotClass(t *testing.T) {
	(&sliptest.Function{
		Source: `(class-name t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(class-name 'not-a-class)`,
		Panics: true,
	}).Test(t)
}
