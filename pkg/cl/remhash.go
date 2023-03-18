// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Remhash{Function: slip.Function{Name: "remhash", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "remhash",
			Args: []*slip.DocArg{
				{
					Name: "key",
					Type: "object",
					Text: "The key for the _hash-table_ entry.",
				},
				{
					Name: "hash-table",
					Type: "hash-table",
					Text: "The _hash-table_ to remove the value associated with the _key_.",
				},
			},
			Return: "boolean",
			Text: `__remhash__ returns _t_ if value associated with the _key_ is removed
from _hash-table_ and _nil_ otherwise.`,
			Examples: []string{
				"(setq table (make-hash-table))",
				"(setf (gethash 'a table) 1) => 1",
				"(gethash 'a table) => 1, t",
				"(remhash 'a table) => t",
				"(gethash 'a table) => nil, nil",
			},
		}, &slip.CLPkg)
}

// Remhash represents the remhash function.
type Remhash struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Remhash) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	ht, ok := args[1].(slip.HashTable)
	if !ok {
		slip.PanicType("hash-table", args[1], "hash-table")
	}
	_, has := ht[args[0]]
	delete(ht, args[0])
	if has {
		return slip.True
	}
	return nil
}
