// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestJoinStrings(t *testing.T) {
	(&sliptest.Function{
		Source: `(join " " "abc" 'def)`,
		Expect: `"abc def"`,
	}).Test(t)
}

func TestJoinList(t *testing.T) {
	(&sliptest.Function{
		Source: `(join " " '("abc" def))`,
		Expect: `"abc def"`,
	}).Test(t)
}

func TestJoinBadSeparator(t *testing.T) {
	(&sliptest.Function{
		Source: `(join t '("abc" def))`,
		Panics: true,
	}).Test(t)
}

func TestJoinBadArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(join "" '(t def))`,
		Panics: true,
	}).Test(t)
}
