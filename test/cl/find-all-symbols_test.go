// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFindAllSymbols(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (result)
                  (intern "car" (make-package 'temp :use nil))
				  (setq result (find-all-symbols "car"))
                  (delete-package 'temp)
                  result)`,
		Expect: "(car temp::car)",
	}).Test(t)
}
