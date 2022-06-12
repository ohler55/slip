// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestHashTableSymbol(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.HashTable{slip.Symbol("a"): slip.True},
		String:    "#<HASH-TABLE :COUNT 1>",
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
		String:    "#<HASH-TABLE :COUNT 1>",
		Simple:    map[string]interface{}{"a": nil},
		Hierarchy: "hash-table.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.HashTable{slip.Symbol("a"): nil}, Expect: true},
			{Other: slip.HashTable{slip.Symbol("a"): slip.True}, Expect: false},
			{Other: slip.True, Expect: false},
		},
		Eval: slip.HashTable{slip.Symbol("a"): nil},
	}).Test(t)
}

func TestHashTableString(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.HashTable{slip.String("a"): slip.True},
		String:    "#<HASH-TABLE :COUNT 1>",
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
		String:    "#<HASH-TABLE :COUNT 1>",
		Simple:    map[string]interface{}{"a": nil},
		Hierarchy: "hash-table.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.HashTable{slip.String("a"): nil}, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: slip.HashTable{slip.String("a"): nil},
	}).Test(t)
}

func TestHashTableNonKey(t *testing.T) {
	(&sliptest.Object{
		Target: slip.HashTable{slip.Fixnum(1): slip.True},
		String: "#<HASH-TABLE :COUNT 1>",
		Simple: fmt.Errorf("can not simplify"),
		Eval:   slip.HashTable{slip.Fixnum(1): slip.True},
	}).Test(t)
}
