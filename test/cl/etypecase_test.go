// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEtypecaseSimple(t *testing.T) {
	(&sliptest.Function{
		Source: `(etypecase 3
                  (float 'float)
                  (fixnum 'fixnum))`,
		Expect: "fixnum",
	}).Test(t)
}

func TestEtypecaseListKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(etypecase '(3 2.5)
                  (float 'float)
                  (fixnum 'fixnum))`,
		Expect: "float",
	}).Test(t)
}

func TestEtypecaseOtherwise(t *testing.T) {
	(&sliptest.Function{
		Source: `(etypecase 'x
                  (float 'float)
                  (fixnum 'fixnum)
                  (otherwise 'other))`,
		Expect: "other",
	}).Test(t)
}

func TestEtypecaseTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(etypecase 'x
                  (float 'float)
                  (fixnum 'fixnum)
                  (t 'other))`,
		Expect: "other",
	}).Test(t)
}

func TestEtypecaseNull(t *testing.T) {
	(&sliptest.Function{
		Source: `(etypecase nil
                  (null 'null)
                  (fixnum 'fixnum))`,
		Expect: "null",
	}).Test(t)
	(&sliptest.Function{
		Source: `(etypecase '(nil)
                  (null 'null)
                  (fixnum 'fixnum))`,
		Expect: "null",
	}).Test(t)
}

func TestEtypecaseNoMatch(t *testing.T) {
	(&sliptest.Function{
		Source: `(etypecase 'x
                  (float 'float)
                  (fixnum 'fixnum))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestEtypecaseBadClause(t *testing.T) {
	(&sliptest.Function{
		Source:    `(etypecase 'x t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(etypecase 'x (7 nil))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
