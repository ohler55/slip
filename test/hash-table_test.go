// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestHashTableSymbol(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.HashTable{slip.Symbol("a"): slip.True},
		String:    "#<hash-table eql 1/-->",
		Simple:    map[string]interface{}{"a": true},
		Hierarchy: "hash-table.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.HashTable{slip.Symbol("a"): slip.True}, Expect: true},
			{Other: slip.HashTable{slip.Symbol("a"): nil}, Expect: false},
			{Other: slip.HashTable{slip.Symbol("b"): nil}, Expect: false},
			{Other: slip.True, Expect: false},
		},
		Eval: slip.HashTable{slip.Symbol("a"): slip.True},
	}).Test(t)
	(&sliptest.Object{
		Target:    slip.HashTable{slip.Symbol("a"): nil},
		String:    "#<hash-table eql 1/-->",
		Simple:    map[string]interface{}{"a": nil},
		Hierarchy: "hash-table.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.HashTable{slip.Symbol("a"): nil}, Expect: true},
			{Other: slip.HashTable{slip.Symbol("a"): slip.True}, Expect: false},
			{Other: slip.True, Expect: false},
		},
		Eval: slip.HashTable{slip.Symbol("a"): nil},
	}).Test(t)
	tt.Equal(t, 2, slip.HashTable{slip.Symbol("a"): slip.True, slip.Symbol("b"): nil}.Length())
}

func TestHashTableString(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.HashTable{slip.String("a"): slip.True},
		String:    "#<hash-table eql 1/-->",
		Simple:    map[string]interface{}{"a": true},
		Hierarchy: "hash-table.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.HashTable{slip.String("a"): slip.True}, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: slip.HashTable{slip.String("a"): slip.True},
	}).Test(t)
	(&sliptest.Object{
		Target:    slip.HashTable{slip.String("a"): nil},
		String:    "#<hash-table eql 1/-->",
		Simple:    map[string]interface{}{"a": nil},
		Hierarchy: "hash-table.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.HashTable{slip.String("a"): nil}, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: slip.HashTable{slip.String("a"): nil},
	}).Test(t)
}

func TestHashTableMixedKey(t *testing.T) {
	(&sliptest.Object{
		Target: slip.HashTable{slip.Fixnum(1): slip.True, slip.Symbol("a"): slip.Fixnum(3), nil: nil},
		String: "#<hash-table eql 3/-->",
		Simple: map[string]any{"1": true, "a": 3, "nil": nil},
		Eval:   slip.HashTable{slip.Fixnum(1): slip.True, slip.Symbol("a"): slip.Fixnum(3), nil: nil},
	}).Test(t)
}

func TestHashTableLoadForm(t *testing.T) {
	table := slip.HashTable{slip.Fixnum(1): slip.True, slip.Symbol("a"): slip.Fixnum(3), nil: nil}
	sliptest.LoadForm(t, table)
}
