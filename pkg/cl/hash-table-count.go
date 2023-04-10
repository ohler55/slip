// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := HashTableCount{Function: slip.Function{Name: "hash-table-count", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "hash-table-count",
			Args: []*slip.DocArg{
				{
					Name: "hash-table",
					Type: "hash-table",
					Text: "The _hash-table_ to return the number of entries of.",
				},
			},
			Return: "fixnum",
			Text:   `__hash-table-count__ returns number of entries in the _hash-table_.`,
			Examples: []string{
				"(hash-table-count (make-hash-table)) => 0",
			},
		}, &slip.CLPkg)
}

// HashTablep represents the hash-table-count function.
type HashTableCount struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *HashTableCount) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	ht, ok := args[0].(slip.HashTable)
	if !ok {
		slip.PanicType("hash-table", args[0], "hash-table")
	}
	return slip.Fixnum(len(ht))
}
