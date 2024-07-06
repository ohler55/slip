// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPsetfSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((a 1)
                       (b 2))
                  (psetf a 3 b (+ a 4))
                  (list a b))`,
		Expect: "(3 5)",
	}).Test(t)
}

func TestPsetfPlacer(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((a '(1 2))
                       (b '(2 3)))
                  (psetf (car a) 3 (car b) (+ (car a) 4))
                  (list a b))`,
		Expect: "((3 2) (5 3))",
	}).Test(t)
}

func TestPsetfPlacer2(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((a '((1) 2))
                       (b '((2) 3)))
                  (psetf (car (car a)) 3 (car (car b)) (+ (car (car a)) 4))
                  (list a b))`,
		Expect: "(((3) 2) ((5) 3))",
	}).Test(t)
}

func TestPsetfValues(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((a 1)
                       (b 2))
                  (psetf a 3 b (values 3 4))
                  (list a b))`,
		Expect: "(3 3)",
	}).Test(t)
}

func TestPsetfOdd(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((a 1)
                       (b 2))
                  (psetf a 3 b))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestPsetfNotPlacer(t *testing.T) {
	(&sliptest.Function{
		Source:    `(psetf t 3)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
