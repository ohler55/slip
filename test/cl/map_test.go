// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMapListOne(t *testing.T) {
	(&sliptest.Function{
		Source: `(map 'list (lambda (x) (1+ x)) '(1 2 3))`,
		Expect: "(2 3 4)",
	}).Test(t)
}

func TestMapStringOne(t *testing.T) {
	(&sliptest.Function{
		Source: `(map 'string 'code-char '(65 66 67))`,
		Expect: `"ABC"`,
	}).Test(t)
}

func TestMapVectorOne(t *testing.T) {
	(&sliptest.Function{
		Source: `(map 'vector (lambda (x) (1+ x)) '(1 2 3))`,
		Array:  true,
		Expect: "#(2 3 4)",
	}).Test(t)
}

func TestMapOctetsOne(t *testing.T) {
	(&sliptest.Function{
		Source: `(map 'octets (lambda (x) (1+ x)) '(1 2 3))`,
		Array:  true,
		Expect: "#(2 3 4)",
	}).Test(t)
}

func TestMapListTwo(t *testing.T) {
	(&sliptest.Function{
		Source: `(map 'list #'+ '(1 2 3 4) '(1 2 3))`,
		Expect: "(2 4 6)",
	}).Test(t)
}

func TestMapNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(map 'print t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMapNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source:    `(map t #'+ '(1 2 3) '(2 3 4))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
