// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMultipleValueBindBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(multiple-value-bind (x y) (floor 130 11) (list x y))`,
		Expect: "(11 9)",
	}).Test(t)
}

func TestMultipleValueBindTooFew(t *testing.T) {
	(&sliptest.Function{
		Source: `(multiple-value-bind (x) (floor 130 11) x)`,
		Expect: "11",
	}).Test(t)
}

func TestMultipleValueBindTooMany(t *testing.T) {
	(&sliptest.Function{
		Source: `(multiple-value-bind (x y z) (floor 130 11) (list x y z))`,
		Expect: "(11 9 nil)",
	}).Test(t)
}

func TestMultipleValueBindSingle(t *testing.T) {
	(&sliptest.Function{
		Source: `(multiple-value-bind (x y) (+ 1 2) (list x y))`,
		Expect: "(3 nil)",
	}).Test(t)
}

func TestMultipleValueBindReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(block nil (multiple-value-bind (x y) (floor 130 11) (return 7) (list x y)))`,
		Expect: "7",
	}).Test(t)
}

func TestMultipleValueBindGo(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (z)
                  (tagbody
                   (multiple-value-bind (x y) (floor 130 11)
                    (setq z (list x y))
                    (go skip)
                    (setq z 0))
                   skip)
                  z)`,
		Expect: "(11 9)",
	}).Test(t)
}

func TestMultipleValueBindBadVars(t *testing.T) {
	(&sliptest.Function{
		Source:    `(multiple-value-bind (t) (+ 1 2) (list x y))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(multiple-value-bind t (+ 1 2) (list x y))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
