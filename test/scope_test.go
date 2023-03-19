// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestScopeLet(t *testing.T) {
	sym := slip.Symbol("x")
	var parent slip.Scope
	parent.Let(sym, slip.Fixnum(1))
	child := parent.NewScope()
	tt.Equal(t, true, child.Has(sym))
	tt.Equal(t, slip.Fixnum(1), child.Get(sym))

	sym = slip.Symbol("multi")
	child.Let(sym, slip.Values{nil, slip.Fixnum(7)})
	tt.Equal(t, true, child.Has(sym))

	tt.Panic(t, func() { child.Let(slip.Symbol("Fixnum"), slip.Fixnum(3)) })
}

func TestScopeSet(t *testing.T) {
	x := slip.Symbol("x")
	y := slip.Symbol("y")
	z := slip.Symbol("z")

	var parent slip.Scope
	parent.Let(x, slip.Fixnum(0))
	child := parent.NewScope()
	child.Let(y, slip.Fixnum(0))

	child.Set(x, slip.Values{slip.Fixnum(1), nil})
	child.Set(y, slip.Fixnum(2))
	child.Set(z, slip.Fixnum(3))

	tt.Equal(t, slip.Fixnum(1), child.Get(x))
	tt.Equal(t, slip.Fixnum(1), parent.Get(x))
	tt.Equal(t, false, slip.CurrentPackage.Has(string(x)))

	tt.Equal(t, slip.Fixnum(2), child.Get(y))
	tt.Equal(t, false, parent.Has(y))
	tt.Equal(t, false, parent.Bound(y))
	tt.Equal(t, false, slip.CurrentPackage.Has(string(y)))

	tt.Equal(t, slip.Fixnum(3), child.Get(z))
	tt.Equal(t, slip.Fixnum(3), parent.Get(z))
	tt.Equal(t, true, slip.CurrentPackage.Has(string(z)))
	tt.Equal(t, true, parent.Bound(z))

	tt.Equal(t, slip.Fixnum(9223372036854775807), child.Get(slip.Symbol("most-positive-fixnum")))

	tt.Panic(t, func() { child.Set(slip.Symbol("Fixnum"), slip.Fixnum(3)) })
}

func TestScopeRemove(t *testing.T) {
	x := slip.Symbol("x")
	y := slip.Symbol("y")

	var parent slip.Scope
	parent.Let(x, slip.Fixnum(0))
	child := parent.NewScope()
	child.Let(y, slip.Fixnum(0))

	child.Set(x, slip.Values{slip.Fixnum(1), nil})
	child.Set(y, slip.Fixnum(2))

	child.Remove(x)
	child.Remove(y)

	tt.Equal(t, false, child.Has(x))
	tt.Equal(t, false, parent.Has(x))

	tt.Equal(t, false, child.Has(y))
	tt.Equal(t, false, parent.Has(y))

	tt.Equal(t, true, child.Has(slip.Symbol("most-positive-fixnum")))
}
