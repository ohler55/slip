// Copyright (c) 2023, Peter Ohler, All rights reserved.

package repl

import (
	"os"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl"
)

func TestTermReader(t *testing.T) {
	err := os.RemoveAll("config/config.lisp")
	tt.Nil(t, err)
	scope := repl.GetScope()
	origOut := scope.Get(slip.Symbol("*standard-output*"))
	origIn := scope.Get(slip.Symbol("*standard-input*"))
	origPrompt := scope.Get(slip.Symbol("*repl-prompt*"))

	defer func() {
		scope.Set(slip.Symbol("*standard-output*"), origOut)
		scope.Set(slip.Symbol("*standard-input*"), origIn)
		scope.Set(slip.Symbol("*repl-prompt*"), origPrompt)
	}()

	var out strings.Builder
	in := strings.NewReader("\n(setq x 3)\n")
	scope.Set(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Set(slip.Symbol("*standard-input*"), &slip.InputStream{Reader: in})
	scope.Set(slip.Symbol("*repl-prompt*"), slip.String("> "))
	scope.Set(slip.Symbol("*repl-editor*"), nil)

	repl.Run()

	tt.Equal(t, "> 3\n> \nBye\n", out.String())
}

func TestTermReaderPanic(t *testing.T) {
	err := os.RemoveAll("config/config.lisp")
	tt.Nil(t, err)
	scope := repl.GetScope()
	origOut := scope.Get(slip.Symbol("*standard-output*"))
	origIn := scope.Get(slip.Symbol("*standard-input*"))
	origPrompt := scope.Get(slip.Symbol("*repl-prompt*"))

	defer func() {
		scope.Set(slip.Symbol("*standard-output*"), origOut)
		scope.Set(slip.Symbol("*standard-input*"), origIn)
		scope.Set(slip.Symbol("*repl-prompt*"), origPrompt)
	}()

	var out strings.Builder
	in := strings.NewReader("\nzzz\n")
	scope.Set(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	scope.Set(slip.Symbol("*standard-input*"), &slip.InputStream{Reader: in})
	scope.Set(slip.Symbol("*repl-prompt*"), slip.String("> "))
	scope.Set(slip.Symbol("*repl-editor*"), nil)

	repl.Run()

	tt.Equal(t, true, strings.Contains(out.String(), "Variable zzz is unbound."))
}
