// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := HashTablep{Function: slip.Function{Name: "hash-table-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "hash-table-p",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "boolean",
			Text:   `__hash-table-p__ returns _true_ if _object_ is a hash-table.`,
			Examples: []string{
				"(hash-table-p (make-hash-table)) => t",
				"(hash-table-p 5.1) => nil",
			},
		}, &slip.CLPkg)
}

// HashTablep represents the hash-table-p function.
type HashTablep struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *HashTablep) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if _, ok := args[0].(slip.HashTable); ok {
		return slip.True
	}
	return nil
}
