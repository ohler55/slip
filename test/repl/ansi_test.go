// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"testing"

	_ "github.com/ohler55/slip/pkg/repl"
	"github.com/ohler55/slip/sliptest"
)

func TestAnsiBasic(t *testing.T) {
	(&sliptest.Function{
		Source:   `(ansi :red)`,
		Expect:   `"\u001b[0;31m"`,
		Readably: true,
	}).Test(t)
}

func TestAnsiBold(t *testing.T) {
	(&sliptest.Function{
		Source:   `(ansi :bold)`,
		Expect:   `"\u001b[1m"`,
		Readably: true,
	}).Test(t)
}

func TestAnsiBoldColor(t *testing.T) {
	(&sliptest.Function{
		Source:   `(ansi :bold :green)`,
		Expect:   `"\u001b[1;32m"`,
		Readably: true,
	}).Test(t)
}

func TestAnsiUnderlineColor(t *testing.T) {
	(&sliptest.Function{
		Source:   `(ansi :underline :yellow)`,
		Expect:   `"\u001b[4;33m"`,
		Readably: true,
	}).Test(t)
}

func TestAnsiBlinkColor(t *testing.T) {
	(&sliptest.Function{
		Source:   `(ansi :blink :blue)`,
		Expect:   `"\u001b[5;34m"`,
		Readably: true,
	}).Test(t)
}

func TestAnsiBrightColor(t *testing.T) {
	(&sliptest.Function{
		Source:   `(ansi :bright :magenta)`,
		Expect:   `"\u001b[0;95m"`,
		Readably: true,
	}).Test(t)
}

func TestAnsiColors(t *testing.T) {
	(&sliptest.Function{
		Source:   `(ansi :cyan)`,
		Expect:   `"\u001b[0;36m"`,
		Readably: true,
	}).Test(t)
	(&sliptest.Function{
		Source:   `(ansi :black)`,
		Expect:   `"\u001b[0;30m"`,
		Readably: true,
	}).Test(t)
	(&sliptest.Function{
		Source:   `(ansi :white)`,
		Expect:   `"\u001b[0;37m"`,
		Readably: true,
	}).Test(t)
}

func TestAnsiReset(t *testing.T) {
	(&sliptest.Function{
		Source:   `(ansi :reset)`,
		Expect:   `"\u001b[m"`,
		Readably: true,
	}).Test(t)
}

func TestAnsiNotKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(ansi t)`,
		Panics: true,
	}).Test(t)
}

func TestAnsiNotValidKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(ansi :bad)`,
		Panics: true,
	}).Test(t)
}
