// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestGoFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let (x)
 (tagbody
  (setq x 1)
  (go 3)
  (setq x (1+ x))
  3
  (setq x (1+ x)))
 x)`,
		Expect: "2",
	}).Test(t)
}

func TestGoTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let (x)
 (tagbody
  (setq x 1)
  (go t)
  (setq x (1+ x))
  t
  (setq x (1+ x)))
 x)`,
		Expect: "2",
	}).Test(t)
}

func TestGoBadTag(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let (x)
 (tagbody
  (setq x 1)
  (go "xyz")
  (setq x (1+ x))
  "xyz"
  (setq x (1+ x)))
 x)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestGoNotInTagbody(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let (x)
 (go 1)
 (setq x (1+ x))
 1
 (setq x (1+ x)))`,
		PanicType: slip.Symbol("control-error"),
	}).Test(t)
}
