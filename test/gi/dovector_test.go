// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDovectorList(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sum 0)) (dovector (x '(1 2 3) sum) (setq sum (+ sum x))))`,
		Expect: "6",
	}).Test(t)
}

func TestDovectorVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sum 0)) (dovector (x #(1 2 3) sum) (setq sum (+ sum x))))`,
		Expect: "6",
	}).Test(t)
}

func TestDovectorOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sum 0)) (dovector (x (coerce #(1 2 3) 'octets) sum) (setq sum (+ sum x))))`,
		Expect: "6",
	}).Test(t)
}

func TestDovectorGo(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sum 0))
                  (dovector (x '(1 2 3) sum)
                   (when (< x 2) (go skip))
                   (setq sum (+ sum x))
                   skip))`,
		Expect: "5",
	}).Test(t)
}

func TestDovectorReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sum 0))
                  (dovector (x '(1 2 3) sum)
                   (when (< 2 x) (return sum))
                   (setq sum (+ sum x))))`,
		Expect: "3",
	}).Test(t)
}

func TestDovectorNestedReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sum 0))
                  (block outer
                   (dovector (x '(1 2 3) sum)
                    (when (< 2 x) (return-from outer sum))
                    (setq sum (+ sum x)))))`,
		Expect: "3",
	}).Test(t)
}

func TestDovectorBadInput(t *testing.T) {
	(&sliptest.Function{
		Source: `(dovector 3)`,
		Panics: true,
	}).Test(t)
}

func TestDovectorBadVar(t *testing.T) {
	(&sliptest.Function{
		Source: `(dovector (t '(1 2 3) x))`,
		Panics: true,
	}).Test(t)
}

func TestDovectorBadList(t *testing.T) {
	(&sliptest.Function{
		Source: `(dovector (x t) x)`,
		Panics: true,
	}).Test(t)
}

func TestDovectorExtraInput(t *testing.T) {
	(&sliptest.Function{
		Source: `(dovector (x '(1 2 3) x 3))`,
		Panics: true,
	}).Test(t)
}

func TestDovectorBadResultForm(t *testing.T) {
	(&sliptest.Function{
		Source: `(dovector (x '(1 2 3) t) x))`,
		Panics: true,
	}).Test(t)
}

func TestDovectorBadReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sum 0))
                  (dovector (x '(1 2 3) sum)
                   (when (< 2 x) (return-from outer sum))
                   (setq sum (+ sum x))))`,
		Panics: true,
	}).Test(t)
}
