// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl"
)

func TestTermock(t *testing.T) {
	tm := repl.NewTermock(40, 80)
	tt.Equal(t, "Termock", tm.String())
	tt.Equal(t, "Termock", string(tm.Append(nil)))
	tt.Nil(t, tm.Simplify())
	tt.Equal(t, false, tm.Equal(nil))
	tt.Equal(t, []slip.Symbol{slip.Symbol("termock")}, tm.Hierarchy())
	tt.Nil(t, tm.Eval(nil, 0))
}
