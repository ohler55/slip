// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDotimesBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sum 0)) (dotimes (x 3 (* sum x)) (setq sum (+ sum x))))`,
		Expect: "9",
	}).Test(t)
}

func TestDotimesGo(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sum 0))
                  (dotimes (x (+ 1 2) sum)
                   (when (< x 2) (go skip))
                   (setq sum (+ sum x))
                   skip))`,
		Expect: "2",
	}).Test(t)
}

func TestDotimesReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sum 0))
                  (dotimes (x 4 sum)
                   (when (< 2 x) (return sum))
                   (setq sum (+ sum x))))`,
		Expect: "3",
	}).Test(t)
}

func TestDotimesNestedReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sum 0))
                  (block outer
                   (dotimes (x 4 sum)
                    (when (< 2 x) (return-from outer sum))
                    (setq sum (+ sum x)))))`,
		Expect: "3",
	}).Test(t)
}

func TestDotimesBadInput(t *testing.T) {
	(&sliptest.Function{
		Source: `(dotimes 3)`,
		Panics: true,
	}).Test(t)
}

func TestDotimesBadVar(t *testing.T) {
	(&sliptest.Function{
		Source: `(dotimes (t 3 x))`,
		Panics: true,
	}).Test(t)
}

func TestDotimesBadList(t *testing.T) {
	(&sliptest.Function{
		Source: `(dotimes (x t) x)`,
		Panics: true,
	}).Test(t)
}

func TestDotimesExtraInput(t *testing.T) {
	(&sliptest.Function{
		Source: `(dotimes (x 3 x 3))`,
		Panics: true,
	}).Test(t)
}

func TestDotimesBadResultForm(t *testing.T) {
	(&sliptest.Function{
		Source: `(dotimes (x 3 t) x))`,
		Panics: true,
	}).Test(t)
}

func TestDotimesBadReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sum 0))
                  (dotimes (x 3 sum)
                   (when (< 1 x) (return-from outer sum))
                   (setq sum (+ sum x))))`,
		Panics: true,
	}).Test(t)
}
