// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestStrcatOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(strcat "abc" #\d '(#\e #\f))`,
		Expect: `"abcdef"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(strcat "abc" #\d (vector #\e #\f))`,
		Expect: `"abcdef"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(strcat "abc" #\d (coerce '(#\e #\f) 'octets))`,
		Expect: `"abcdef"`,
	}).Test(t)
}

func TestStrcatBadString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(strcat t "b")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestStrcatBadCharList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(strcat "b" '(t))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestStrcatBadCharVector(t *testing.T) {
	(&sliptest.Function{
		Source:    `(strcat "b" '#(t))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
