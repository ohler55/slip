// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMakeStringInputStreamFull(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-char (make-string-input-stream "abc"))`,
		Expect: `#\a`,
	}).Test(t)
}

func TestMakeStringInputStart(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-char (make-string-input-stream "abc" 1))`,
		Expect: `#\b`,
	}).Test(t)
}

func TestMakeStringInputStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-char (make-string-input-stream "abc" 1 2))`,
		Expect: `#\b`,
	}).Test(t)
}

func TestMakeStringInputNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-string-input-stream t)`,
		Panics: true,
	}).Test(t)
}

func TestMakeStringInputStartNotFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-string-input-stream "abc" t)`,
		Panics: true,
	}).Test(t)
}

func TestMakeStringInputEndNotFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-string-input-stream "abc" 1 t)`,
		Panics: true,
	}).Test(t)
}

func TestMakeStringInputStartOut(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-string-input-stream "abc" 4)`,
		Panics: true,
	}).Test(t)
}

func TestMakeStringInputEndOut(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-string-input-stream "abc" 1 4)`,
		Panics: true,
	}).Test(t)
}

func TestMakeStringInputEndBeforeStart(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-string-input-stream "abc" 2 1)`,
		Panics: true,
	}).Test(t)
}
