// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

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
