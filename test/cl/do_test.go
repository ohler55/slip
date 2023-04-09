// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDoSimple(t *testing.T) {
	(&sliptest.Function{
		Source: `(do ((x 0 (1+ x))
                      (y 0 (1- y)))
                     ((> (- x y) 5) x))`,
		Expect: "3",
	}).Test(t)
}

func TestDoReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(do ((x 0 (1+ x))
                      (y 0 (1- y)))
                     ((> (- x y) 5) x)
                  (return-from nil 7))`,
		Expect: "7",
	}).Test(t)
}

func TestDoReturnHandoff(t *testing.T) {
	(&sliptest.Function{
		Source: `(block done
                  (do ((x 0 (1+ x))
                       (y 0 (1- y)))
                      ((> (- x y) 5) x)
                   (return-from done 7))
                  8)`,
		Expect: "7",
	}).Test(t)
}

func TestDoBindNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(do ((x)
                      (y 0 (1+ y)))
                     ((null x) y))`,
		Expect: "0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(do (x
                      (y 0 (1+ y)))
                     ((null x) y))`,
		Expect: "0",
	}).Test(t)
}

func TestDoBadBindings(t *testing.T) {
	(&sliptest.Function{
		Source: `(do t ((> (- x y) 5) x))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(do (t
                      (y 0 (1- y)))
                     ((> (- x y) 5) x))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(do ((t)) ((> (- x y) 5) x))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(do (()) ((> (- x y) 5) x))`,
		Panics: true,
	}).Test(t)
}

func TestDoBadTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(do ((x 0 (1+ x))) t)`,
		Panics: true,
	}).Test(t)
}

func TestDoBadReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(do ((x 0 (1+ x))
                      (y 0 (1- y)))
                     ((> (- x y) 5) x)
                  (return-from done 7))`,
		Panics: true,
	}).Test(t)
}

// TBD with multiple forms for result
// TBD with no steps - use body for setting up condition
// TBD validate parallel assignment
// TBD go and tag
