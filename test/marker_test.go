// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestMarker(t *testing.T) {
	m := slip.NewMarker('x')

	tt.Equal(t, "x", m.String())
	tt.Equal(t, "x", string(m.Append(nil)))
	tt.Equal(t, "x", m.Simplify())
	tt.Equal(t, true, m.Equal(slip.NewMarker('x')))
	tt.Equal(t, []slip.Symbol{slip.Symbol("t")}, m.Hierarchy())
	tt.Equal(t, m, m.Eval(nil, 0))
}
