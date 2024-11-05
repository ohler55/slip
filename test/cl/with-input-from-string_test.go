// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWithInputFromStringBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-input-from-string (s "abc def") (read s))`,
		Expect: "abc",
	}).Test(t)
}

func TestWithInputFromStringStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-input-from-string (s "オーラ \"ピーター\" def" :start 4 :end 10) (read s))`,
		Expect: `"ピーター"`,
	}).Test(t)
}

func TestWithInputFromStringIndex(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (i)
                  (list (with-input-from-string (s "オーラ \"ピーター\" def" :start 4 :end 13 :index i) (read s)) i))`,
		Expect: `("ピーター" 4)`,
	}).Test(t)
}

func TestWithInputFromStringBadArgs(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-input-from-string t (read s))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithInputFromStringBadVar(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-input-from-string (t "abc") (read s))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithInputFromStringNotString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-input-from-string (x t) (read s))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithInputFromStringBadStart(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-input-from-string (x "abc" :start t) (read s))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithInputFromStringBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-input-from-string (x "abc" :end t) (read s))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
