// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestSearchNilNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(search nil nil)",
		Expect: "0",
	}).Test(t)
}

func TestSearchNilList(t *testing.T) {
	(&sliptest.Function{
		Source: "(search nil '(a b c d))",
		Expect: "0",
	}).Test(t)
}

func TestSearchListNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(search '(b c) nil)",
		Expect: "nil",
	}).Test(t)
}

func TestSearchListList(t *testing.T) {
	(&sliptest.Function{
		Source: "(search '(b c) '(a b c d))",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(search '(d e) '(a b c d))",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(search '(b x) '(a b c d))",
		Expect: "nil",
	}).Test(t)
}

func TestSearchVector(t *testing.T) {
	(&sliptest.Function{
		Source: "(search #(b c) #(a b c d))",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(search #(b c) '(a b c d))",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(search '(b c) #(a b c d))",
		Expect: "1",
	}).Test(t)
}

func TestSearchStringList(t *testing.T) {
	(&sliptest.Function{
		Source: `(search "" '(a b c d))`,
		Expect: "0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(search "a" '(a b c d))`,
		Expect: "nil",
	}).Test(t)
}
