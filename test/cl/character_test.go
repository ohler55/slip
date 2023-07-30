// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharacterCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(character #\a)`,
		Expect: `#\a`,
	}).Test(t)
}

func TestCharacterString(t *testing.T) {
	(&sliptest.Function{
		Source: `(character "a")`,
		Expect: `#\a`,
	}).Test(t)
}

func TestCharacterSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(character 'a)`,
		Expect: `#\a`,
	}).Test(t)
}

func TestCharacterTooLong(t *testing.T) {
	(&sliptest.Function{
		Source: `(character "abc")`,
		Panics: true,
	}).Test(t)
}

func TestCharacterNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(character 7)`,
		Panics: true,
	}).Test(t)
}
