// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTypecaseSimple(t *testing.T) {
	(&sliptest.Function{
		Source: `(typecase 3
                  (float 'float)
                  (fixnum 'fixnum))`,
		Expect: "fixnum",
	}).Test(t)
}

func TestTypecaseListKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(typecase '(3 2.5)
                  (float 'float)
                  (fixnum 'fixnum))`,
		Expect: "float",
	}).Test(t)
}

func TestTypecaseOtherwise(t *testing.T) {
	(&sliptest.Function{
		Source: `(typecase 'x
                  (float 'float)
                  (fixnum 'fixnum)
                  (otherwise 'other))`,
		Expect: "other",
	}).Test(t)
}

func TestTypecaseTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(typecase 'x
                  (float 'float)
                  (fixnum 'fixnum)
                  (t 'other))`,
		Expect: "other",
	}).Test(t)
}

func TestTypecaseNull(t *testing.T) {
	(&sliptest.Function{
		Source: `(typecase nil
                  (null 'null)
                  (fixnum 'fixnum))`,
		Expect: "null",
	}).Test(t)
	(&sliptest.Function{
		Source: `(typecase '(nil)
                  (null 'null)
                  (fixnum 'fixnum))`,
		Expect: "null",
	}).Test(t)
}

func TestTypecaseNoMatch(t *testing.T) {
	(&sliptest.Function{
		Source: `(typecase 'x
                  (float 'float)
                  (fixnum 'fixnum))`,
		Expect: "nil",
	}).Test(t)
}

func TestTypecaseBadClause(t *testing.T) {
	(&sliptest.Function{
		Source:    `(typecase 'x t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(typecase 'x (7 nil))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
