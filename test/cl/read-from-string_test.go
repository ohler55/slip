// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestReadFromStringDefaults(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-from-string " 123 ")`,
		Expect: `123, 5`,
	}).Test(t)
}

func TestReadFromStringPreserve(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-from-string " 123 " :preserve-whitespace t)`,
		Expect: `123, 4`,
	}).Test(t)
}

func TestReadFromStringList(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-from-string " (1 2) ")`,
		Expect: `(1 2), 7`,
	}).Test(t)
}

func TestReadFromStringStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-from-string "x 123 y" :start 2 :end 6)`,
		Expect: `123, 5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(read-from-string "x 123 y" :start 7 :end 5)`,
		Panics: true,
	}).Test(t)
}

func TestReadFromStringAfter(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-from-string "123 abc" )`,
		Expect: `123, 4`,
	}).Test(t)
}

func TestReadFromStringEofpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-from-string " " t)`,
		Panics: true,
	}).Test(t)
}

func TestReadFromStringEofv(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-from-string " " nil 'broken)`,
		Expect: `broken, 1`,
	}).Test(t)
}

func TestReadFromStringEof(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-from-string " \"abc")`,
		Panics: true,
	}).Test(t)
}

func TestReadFromStringNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-from-string t)`,
		Panics: true,
	}).Test(t)
}

func TestReadFromStringStartNotFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-from-string "123" :start t)`,
		Panics: true,
	}).Test(t)
}

func TestReadFromStringEndNotFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-from-string "123" :end t)`,
		Panics: true,
	}).Test(t)
}

func TestReadFromStringBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-from-string "123" :bad t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(read-from-string "123" nil nil t)`,
		Panics: true,
	}).Test(t)
}
