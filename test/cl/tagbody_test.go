// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestTagbodyEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(tagbody)`,
		Expect: "nil",
	}).Test(t)
}

func TestTagbodyNoTag(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x 0)) (tagbody (setq x (1+ x)) (setq x (1+ x))) x)`,
		Expect: "2",
	}).Test(t)
}

func TestTagbodyGoTag(t *testing.T) {
	(&sliptest.Function{
		Source: `
(let (x)
 (tagbody
  (setq x 1)
  (go skip)
  (setq x (1+ x))
  skip
  (setq x (1+ x)))
 x)`,
		Expect: "2",
	}).Test(t)
}
