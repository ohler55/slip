// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestProgEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(prog nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestProgSimple(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((y 0)) (list (prog ((x 0)) (setq y (+ 1 x))) y))`,
		Expect: "(nil 1)",
	}).Test(t)
}

func TestProgGoReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(prog ((x 0))
                  (setq x (1+ x))
                  (go skip)
                  (setq x (1+ x))
                  skip
                  (return x))`,
		Expect: "1",
	}).Test(t)
}

func TestProgReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(block outer
                  (prog ((x 0))
                   (setq x (1+ x))
                   (return-from outer x)))`,
		Expect: "1",
	}).Test(t)
}

func TestProgBadReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(prog ((x 0))
                  (setq x (1+ x))
                  (return-from outer x))`,
		Panics: true,
	}).Test(t)
}

func TestProgArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(prog)`,
		Panics: true,
	}).Test(t)
}
