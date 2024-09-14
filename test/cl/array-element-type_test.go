// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestArrayElementTypeVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-element-type (make-array 4))`,
		Expect: "t",
	}).Test(t)
}

func TestArrayElementTypeArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-element-type (make-array '(2 3) :element-type 'fixnum))`,
		Expect: "fixnum",
	}).Test(t)
}

func TestArrayElementTypeOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-element-type (coerce "abc" 'octets))`,
		Expect: "octet",
	}).Test(t)
}

func TestArrayElementTypeNotArray(t *testing.T) {
	(&sliptest.Function{
		Source:    `(array-element-type t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
