// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestGetBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(get '(a 1 b 2 c 3) 'b)`,
		Expect: "2",
	}).Test(t)
}

func TestGetDefault(t *testing.T) {
	(&sliptest.Function{
		Source: `(get '(a 1 b 2 c 3) 'd 5)`,
		Expect: "5",
	}).Test(t)
}

func TestGetNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(get t 'a)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestGetSetfReplace(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((plist '(a 1 b 2 c 3)))
                  (list (setf (get plist 'b) 4) plist))`,
		Expect: "(4 (a 1 b 4 c 3))",
	}).Test(t)
}

func TestGetSetfAdd(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((plist '(a 1 b 2 c 3)))
                  (list (setf (get plist 'd) 4) plist))`,
		Expect: "(4 (a 1 b 2 c 3 d 4))",
	}).Test(t)
}

func TestGetSetfNotPlist(t *testing.T) {
	(&sliptest.Function{
		Source:    `(setf (get t 'd) 4)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (get nil 'd) 4)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
