// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Clrhash{Function: slip.Function{Name: "clrhash", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "clrhash",
			Args: []*slip.DocArg{
				{
					Name: "hash-table",
					Type: "hash-table",
					Text: "The _hash-table_ to clear.",
				},
			},
			Return: "hash-table",
			Text:   `__clrhash__ removes all entries from _hash-table_.`,
			Examples: []string{
				"(setq table (make-hash-table))",
				"(setf (gethash 'a table) 1) => 1",
				"(clrhash table) => #<hash-table eql 0/-->",
			},
		}, &slip.CLPkg)
}

// Clrhash represents the clrhash function.
type Clrhash struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Clrhash) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	ht, ok := args[0].(slip.HashTable)
	if !ok {
		slip.PanicType("hash-table", args[0], "hash-table")
	}
	for k := range ht {
		delete(ht, k)
	}
	return ht
}
