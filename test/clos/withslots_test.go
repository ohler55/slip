// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/clos"
)

func TestWithSlots(t *testing.T) {
	ws := clos.WithSlots{Vars: map[string]slip.Object{"quux": nil}}

	ws.SetSynchronized(false)
	ws.SetSlotValue(slip.Symbol("quux"), slip.Fixnum(5))
	simple := ws.Simplify()
	jp.C("id").Del(simple)
	tt.Equal(t, "{vars: {quux: 5}}", pretty.SEN(simple))
	value, has := ws.SlotValue(slip.Symbol("quux"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Fixnum(5), value)
	tt.Equal(t, []string{"quux"}, ws.SlotNames())
	tt.Equal(t, false, ws.Synchronized())

	ws.SetSynchronized(true)
	ws.SetSlotValue(slip.Symbol("quux"), slip.Fixnum(7))
	value, has = ws.SlotValue(slip.Symbol("quux"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Fixnum(7), value)
	tt.Equal(t, true, ws.Synchronized())
}
