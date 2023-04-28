// Copyright (c) 2023, Peter Ohler, All rights reserved.

package repl

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestHelpTop(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(help)`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, true, strings.Contains(out.String(), "SLIce Processing (SLIP) is an ANSI Common LISP written in go"))
}

func TestHelpIndex(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(help 'index)`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, true, strings.Contains(out.String(), "SLIP REPL Help Index"))
}

func TestHelpConfig(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(help "config")`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, true, strings.Contains(out.String(), "SLIP REPL Configuration"))
}

func TestHelpEdit(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(help "edit")`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, true, strings.Contains(out.String(), "SLIP REPL Editing"))
}

func TestHelpHistory(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(help 'history)`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, true, strings.Contains(out.String(), "SLIP REPL History"))
}

func TestHelpUseful(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(help 'useful)`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, true, strings.Contains(out.String(), "Useful SLIP Functions"))
}

func TestHelpUnknownTopic(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(help 'what)`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, true, strings.Contains(out.String(), "SLIP REPL Help Index"))
}

func TestHelpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(help 'useful t)`,
		Panics: true,
	}).Test(t)
}

func TestHelpBadTopic(t *testing.T) {
	(&sliptest.Function{
		Source: `(help t)`,
		Panics: true,
	}).Test(t)
}
