// Copyright (c) 2024, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl"
	"github.com/ohler55/slip/sliptest"
)

func TestShowHistoryPlain(t *testing.T) {
	repl.TheHistory.SetLimit(100)
	repl.TheHistory.Load("config/history")
	repl.TheHistory.Clear(0, -1)
	repl.TheHistory.Add(repl.NewForm([]byte("(+ 1 2)")))
	(&sliptest.Function{
		Source: `(show-history nil)`,
		Expect: `"
(+ 1 2)
"`,
	}).Test(t)
}

func TestShowHistoryOptions(t *testing.T) {
	repl.TheHistory.SetLimit(100)
	repl.TheHistory.Load("config/history")
	repl.TheHistory.Clear(0, -1)
	repl.TheHistory.Add(repl.NewForm([]byte("(+ 1 2)")))
	repl.TheHistory.Add(repl.NewForm([]byte("(* 2 3)")))
	(&sliptest.Function{
		Source: `(show-history nil :annotate t :tight t :start 1 :end -1)`,
		Expect: `";; 2
(* 2 3)
"`,
	}).Test(t)
}

func TestShowHistoryStdout(t *testing.T) {
	repl.TheHistory.SetLimit(100)
	repl.TheHistory.Load("config/history")
	repl.TheHistory.Clear(0, -1)
	repl.TheHistory.Add(repl.NewForm([]byte("(+ 1 2)")))
	repl.TheHistory.Add(repl.NewForm([]byte("(* 2 3)")))
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(show-history :raw t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, `(+ 1 2)
(* 2 3)
`, out.String())

	out.Reset()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(show-history t :raw t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, `(+ 1 2)
(* 2 3)
`, out.String())
}

func TestShowHistoryStream(t *testing.T) {
	repl.TheHistory.SetLimit(100)
	repl.TheHistory.Load("config/history")
	repl.TheHistory.Clear(0, -1)
	repl.TheHistory.Add(repl.NewForm([]byte("(+ 1 2)")))
	repl.TheHistory.Add(repl.NewForm([]byte("(* 2 3)")))
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("out", &slip.OutputStream{Writer: &out})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(show-history out :tight t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, `(+ 1 2)
(* 2 3)
`, out.String())
}

func TestShowHistoryBadStart(t *testing.T) {
	(&sliptest.Function{
		Source:    `(show-history nil :start t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestShowHistoryBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(show-history nil :end t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestShowHistoryBadDestination(t *testing.T) {
	(&sliptest.Function{
		Source:    `(show-history 7 :start t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

type badWriter int

func (w badWriter) Write([]byte) (int, error) {
	return 0, fmt.Errorf("oops")
}

func TestShowHistoryStreamError(t *testing.T) {
	repl.TheHistory.SetLimit(100)
	repl.TheHistory.Load("config/history")
	repl.TheHistory.Clear(0, -1)
	repl.TheHistory.Add(repl.NewForm([]byte("(+ 1 2)")))
	scope := slip.NewScope()
	scope.Let("out", &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(show-history out :tight t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}
