// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDolistBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sum 0)) (dolist (x '(1 2 3) sum) (setq sum (+ sum x))))`,
		Expect: "6",
	}).Test(t)
}

func TestDolistGo(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sum 0))
                  (dolist (x '(1 2 3) sum)
                   (when (< x 2) (go skip))
                   (setq sum (+ sum x))
                   skip))`,
		Expect: "5",
	}).Test(t)
}

func TestDolistReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sum 0))
                  (dolist (x '(1 2 3) sum)
                   (when (< 2 x) (return sum))
                   (setq sum (+ sum x))))`,
		Expect: "3",
	}).Test(t)
}

func TestDolistNestedReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sum 0))
                  (block outer
                   (dolist (x '(1 2 3) sum)
                    (when (< 2 x) (return-from outer sum))
                    (setq sum (+ sum x)))))`,
		Expect: "3",
	}).Test(t)
}

func TestDolistBadInput(t *testing.T) {
	(&sliptest.Function{
		Source: `(dolist 3)`,
		Panics: true,
	}).Test(t)
}

func TestDolistBadVar(t *testing.T) {
	(&sliptest.Function{
		Source: `(dolist (t '(1 2 3) x))`,
		Panics: true,
	}).Test(t)
}

func TestDolistBadList(t *testing.T) {
	(&sliptest.Function{
		Source: `(dolist (x t) x)`,
		Panics: true,
	}).Test(t)
}

func TestDolistExtraInput(t *testing.T) {
	(&sliptest.Function{
		Source: `(dolist (x '(1 2 3) x 3))`,
		Panics: true,
	}).Test(t)
}

func TestDolistBadResultForm(t *testing.T) {
	(&sliptest.Function{
		Source: `(dolist (x '(1 2 3) t) x))`,
		Panics: true,
	}).Test(t)
}

func TestDolistBadReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sum 0))
                  (dolist (x '(1 2 3) sum)
                   (when (< 2 x) (return-from outer sum))
                   (setq sum (+ sum x))))`,
		Panics: true,
	}).Test(t)
}
