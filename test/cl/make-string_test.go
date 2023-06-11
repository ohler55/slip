// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMakeStringNoChar(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-string 5)`,
		Expect: `""`,
	}).Test(t)
}

func TestMakeStringChar(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-string 5 :initial-element #\b)`,
		Expect: `"bbbbb"`,
	}).Test(t)
}

func TestMakeStringNotPositiveFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-string t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(make-string -2)`,
		Panics: true,
	}).Test(t)
}

func TestMakeStringMissingKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-string 3 :initial-element)`,
		Panics: true,
	}).Test(t)
}

func TestMakeStringBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-string 3 :bad t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(make-string 3 t t)`,
		Panics: true,
	}).Test(t)
}

func TestMakeStringBadInitialElement(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-string 3 :initial-element t)`,
		Panics: true,
	}).Test(t)
}

func TestMakeStringBadElementType(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-string 3 :element-type t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(make-string 3 :element-type 'not-character)`,
		Panics: true,
	}).Test(t)
}
