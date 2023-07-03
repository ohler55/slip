// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestWriteToStringBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(write-to-string 123)`,
		Expect: `"123"`,
	}).Test(t)
}

func TestWriteToStringWithStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(write-to-string 123 :stream nil)`,
		Panics: true,
	}).Test(t)
}

func TestWriteToStringBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(write-to-string 123 :bad nil)`,
		Panics: true,
	}).Test(t)
}
