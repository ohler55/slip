// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip/pkg/gi"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeMutex(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-mutex)`,
		Expect: "/#<mutex [0-9a-f]+>/",
	}).Test(t)
}

func TestMakeMutexObject(t *testing.T) {
	var m gi.Mutex
	tt.Equal(t, "/#<mutex [0-9a-f]+>/", m.String())
	tt.Equal(t, "/#<mutex [0-9a-f]+>/", m.Simplify())
	tt.Equal(t, true, m.Equal(&m))
	tt.Equal(t, "[mutex t]", pretty.SEN(m.Hierarchy()))
	tt.Equal(t, &m, m.Eval(nil, 0))
}
