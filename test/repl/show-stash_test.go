// Copyright (c) 2024, Peter Ohler, All rights reserved.

package repl

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl"
	"github.com/ohler55/slip/sliptest"
)

func TestShowStashPlain(t *testing.T) {
	repl.TheStash.LoadExpanded("config/stash")
	repl.TheStash.Clear(0, -1)
	repl.TheStash.Add(repl.NewForm([]byte("(+ 1 2)")))
	(&sliptest.Function{
		Source: `(show-stash nil)`,
		Expect: `"
(+ 1 2)
"`,
	}).Test(t)
}

func TestShowStashOptions(t *testing.T) {
	repl.TheStash.LoadExpanded("config/stash")
	repl.TheStash.Clear(0, -1)
	repl.TheStash.Add(repl.NewForm([]byte("(+ 1 2)")))
	repl.TheStash.Add(repl.NewForm([]byte("(* 2 3)")))
	(&sliptest.Function{
		Source: `(show-stash nil :annotate t :tight t :start 1 :end -1)`,
		Expect: `";; 2
(* 2 3)
"`,
	}).Test(t)
}

func TestShowStashStdout(t *testing.T) {
	repl.TheStash.LoadExpanded("config/stash")
	repl.TheStash.Clear(0, -1)
	repl.TheStash.Add(repl.NewForm([]byte("(+ 1 2)")))
	repl.TheStash.Add(repl.NewForm([]byte("(* 2 3)")))
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(show-stash :raw t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, `(+ 1 2)
(* 2 3)
`, out.String())

	out.Reset()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(show-stash t :raw t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, `(+ 1 2)
(* 2 3)
`, out.String())
}

func TestShowStashStream(t *testing.T) {
	repl.TheStash.LoadExpanded("config/stash")
	repl.TheStash.Clear(0, -1)
	repl.TheStash.Add(repl.NewForm([]byte("(+ 1 2)")))
	repl.TheStash.Add(repl.NewForm([]byte("(* 2 3)")))
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("out", &slip.OutputStream{Writer: &out})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(show-stash out :tight t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, `(+ 1 2)
(* 2 3)
`, out.String())
}

func TestShowStashBadStart(t *testing.T) {
	(&sliptest.Function{
		Source:    `(show-stash nil :start t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestShowStashBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(show-stash nil :end t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestShowStashBadDestination(t *testing.T) {
	(&sliptest.Function{
		Source:    `(show-stash 7 :start t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestShowStashStreamError(t *testing.T) {
	repl.TheStash.LoadExpanded("config/stash")
	repl.TheStash.Clear(0, -1)
	repl.TheStash.Add(repl.NewForm([]byte("(+ 1 2)")))
	scope := slip.NewScope()
	scope.Let("out", &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(show-stash out :tight t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}
