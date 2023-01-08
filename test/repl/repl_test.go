// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl"
)

func TestREPLMatchColor(t *testing.T) {
	scope := repl.GetScope()
	orig := scope.Get(slip.Symbol("*repl-match-color*"))
	defer func() { scope.Set(slip.Symbol("*repl-match-color*"), orig) }()

	scope.Set(slip.Symbol("*repl-match-color*"), slip.String("x"))
	tt.Equal(t, slip.String("x"), scope.Get(slip.Symbol("*repl-match-color*")))

	tt.Panic(t, func() { scope.Set(slip.Symbol("*repl-match-color*"), slip.True) })
}

func TestREPLWarningPrefix(t *testing.T) {
	scope := repl.GetScope()
	orig := scope.Get(slip.Symbol("*repl-warning-prefix*"))
	defer func() { scope.Set(slip.Symbol("*repl-warning-prefix*"), orig) }()

	scope.Set(slip.Symbol("*repl-warning-prefix*"), slip.String("x"))
	tt.Equal(t, slip.String("x"), scope.Get(slip.Symbol("*repl-warning-prefix*")))

	tt.Panic(t, func() { scope.Set(slip.Symbol("*repl-warning-prefix*"), slip.True) })
}

func TestREPLPrompt(t *testing.T) {
	scope := repl.GetScope()
	orig := scope.Get(slip.Symbol("*repl-prompt*"))
	defer func() { scope.Set(slip.Symbol("*repl-prompt*"), orig) }()

	scope.Set(slip.Symbol("*repl-prompt*"), slip.String("> "))
	tt.Equal(t, slip.String("> "), scope.Get(slip.Symbol("*repl-prompt*")))

	tt.Panic(t, func() { scope.Set(slip.Symbol("*repl-prompt*"), slip.True) })
}

func TestREPLHooks(t *testing.T) {
	slip.UserPkg.Set("foo", slip.Fixnum(7))
	// TBD how can the completer be checked?
	slip.UserPkg.Remove("foo")
	// TBD how can the completer be checked?
}
