// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := HashTableRehashSize{Function: slip.Function{Name: "hash-table-rehash-size", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "hash-table-rehash-size",
			Args: []*slip.DocArg{
				{
					Name: "hash-table",
					Type: "hash-table",
					Text: "The _hash-table-rehash-size_ to return the rehash size of.",
				},
			},
			Return: "fixnum",
			Text:   `__hash-table-rehash-size__ returns 1.`,
			Examples: []string{
				"(hash-table-rehash-size (make-hash-table)) => 1",
			},
		}, &slip.CLPkg)
}

// HashTablep represents the hash-table-rehashSize function.
type HashTableRehashSize struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *HashTableRehashSize) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if _, ok := args[0].(slip.HashTable); !ok {
		slip.TypePanic(s, depth, "hash-table", args[0], "hash-table")
	}
	return slip.Fixnum(1)
}
