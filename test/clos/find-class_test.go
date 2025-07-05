// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFindClassFound(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-class 'vanilla-flavor)`,
		Expect: "#<flavor vanilla-flavor>",
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-class 'fixnum)`,
		Expect: "#<built-in-class fixnum>",
	}).Test(t)
}

func TestFindClassNotFound(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-class 'not-a-class)`,
		Expect: "nil",
	}).Test(t)
}

func TestFindClassNotFoundPanic(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-class 'not-a-class t)`,
		Panics: true,
	}).Test(t)
}

func TestFindClassArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-class)`,
		Panics: true,
	}).Test(t)
}

func TestFindClassNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-class t)`,
		Panics: true,
	}).Test(t)
}
