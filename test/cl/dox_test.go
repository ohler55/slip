// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDoxSimple(t *testing.T) {
	(&sliptest.Function{
		Source: `(do* ((x 0 (1+ x))
                      (y 0 (1- y)))
                     ((> (- x y) 5) x))`,
		Expect: "3",
	}).Test(t)
}

func TestDoxParallel(t *testing.T) {
	// If sequential step evaluation then y should be the same as x.
	(&sliptest.Function{
		Source: `(do* ((x 0 (1+ x))
                      (y 0 x))
                     ((> x 2) y))`,
		Expect: "3",
	}).Test(t)
}

func TestDoxReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(do* ((x 0 (1+ x))
                      (y 0 (1- y)))
                     ((> (- x y) 5) x)
                  (return-from nil 7))`,
		Expect: "7",
	}).Test(t)
}

func TestDoxReturnHandoff(t *testing.T) {
	(&sliptest.Function{
		Source: `(block done
                  (do* ((x 0 (1+ x))
                       (y 0 (1- y)))
                      ((> (- x y) 5) x)
                   (return-from done 7))
                  8)`,
		Expect: "7",
	}).Test(t)
}

func TestDoxBindNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(do* ((x)
                      (y 0 (1+ y)))
                     ((null x) y))`,
		Expect: "0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(do* (x
                      (y 0 (1+ y)))
                     ((null x) y))`,
		Expect: "0",
	}).Test(t)
}

func TestDoxGo(t *testing.T) {
	(&sliptest.Function{
		Source: `(do* ((x 0 (1+ x))
                      (count 0 (1+ count)))
                     ((> x 5) count)
                  (go faster)
                  (setq x (1- x))
                  faster
                  (setq x (1+ x)))`,
		Expect: "3",
	}).Test(t)
}

func TestDoxBadBindings(t *testing.T) {
	(&sliptest.Function{
		Source: `(do* t ((> (- x y) 5) x))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(do* (t
                      (y 0 (1- y)))
                     ((> (- x y) 5) x))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(do* ((t)) ((> (- x y) 5) x))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(do* (()) ((> (- x y) 5) x))`,
		Panics: true,
	}).Test(t)
}

func TestDoxBadTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(do* ((x 0 (1+ x))) t)`,
		Panics: true,
	}).Test(t)
}

func TestDoxBadReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(do* ((x 0 (1+ x))
                      (y 0 (1- y)))
                     ((> (- x y) 5) x)
                  (return-from done 7))`,
		Panics: true,
	}).Test(t)
}
