// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMakeHashTableOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-hash-table)`,
		Expect: "#<hash-table eql 0/-->",
	}).Test(t)
}

func TestMakeHashTableArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-hash-table :test 'eql :size 100 :rehash-size 1.2 :rehash-threshold 1.3 :bogus 2)`,
		Panics: true,
	}).Test(t)
}
