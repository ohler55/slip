// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := HashTableTest{Function: slip.Function{Name: "hash-table-test", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "hash-table-test",
			Args: []*slip.DocArg{
				{
					Name: "hash-table",
					Type: "hash-table",
					Text: "The _hash-table_ to return the test of.",
				},
			},
			Return: "symbol",
			Text:   `__hash-table-test__ returns _eql_.`,
			Examples: []string{
				"(hash-table-test (make-hash-table)) => eql",
			},
		}, &slip.CLPkg)
}

// HashTablep represents the hash-table-test function.
type HashTableTest struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *HashTableTest) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if _, ok := args[0].(slip.HashTable); !ok {
		slip.TypePanic(s, depth, "hash-table", args[0], "hash-table")
	}
	return slip.Symbol("eql")
}
