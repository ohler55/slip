// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := HashTableRehashThreshold{Function: slip.Function{Name: "hash-table-rehash-threshold", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "hash-table-rehash-threshold",
			Args: []*slip.DocArg{
				{
					Name: "hash-table",
					Type: "hash-table",
					Text: "The _hash-table-rehash-threshold_ to return the rehash threshold of.",
				},
			},
			Return: "fixnum",
			Text:   `__hash-table-rehash-threshold__ returns 0.`,
			Examples: []string{
				"(hash-table-rehash-threshold (make-hash-table)) => 0",
			},
		}, &slip.CLPkg)
}

// HashTablep represents the hash-table-rehashThreshold function.
type HashTableRehashThreshold struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *HashTableRehashThreshold) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if _, ok := args[0].(slip.HashTable); !ok {
		slip.PanicType("hash-table", args[0], "hash-table")
	}
	return slip.Fixnum(0)
}
