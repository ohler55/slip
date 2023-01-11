// Copyright (c) 2023, Peter Ohler, All rights reserved.

package repl

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip/pkg/repl"
)

func TestRuneWidth(t *testing.T) {
	tt.Equal(t, 1, repl.RuneWidth('A'))
	tt.Equal(t, 2, repl.RuneWidth('ぴ'))
}
