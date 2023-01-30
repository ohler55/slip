// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Gethash{Function: slip.Function{Name: "gethash", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "gethash",
			Args: []*slip.DocArg{
				{
					Name: "key",
					Type: "object",
					Text: "The key for the _hash-table_ entry.",
				},
				{
					Name: "hash-table",
					Type: "hash-table",
					Text: "The _hash-table_ to get the value associated with the _key_.",
				},
			},
			Return: "object, boolean",
			Text:   `__gethash__ returns the value associated with the _key_ or _nil_ if not in the _hash-table_.`,
			Examples: []string{
				"(setq table (make-hash-table))",
				"(gethash 'a table) => nil, nil",
				"(setf (gethash 'a table) 1) => 1",
				"(gethash 'a table) => 1, t",
			},
		}, &slip.CLPkg)
}

// Gethash represents the gethash function.
type Gethash struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Gethash) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	ht, ok := args[0].(slip.HashTable)
	if !ok {
		slip.PanicType("hash-table", args[0], "hash-table")
	}
	v, has := ht[args[1]]
	var ho slip.Object
	if has {
		ho = slip.True
	}
	return slip.Values{ho, v}
}

// Place a value in the first position of a list or cons.
func (f *Gethash) Place(args slip.List, value slip.Object) {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	ht, ok := args[0].(slip.HashTable)
	if !ok {
		slip.PanicType("hash-table", args[0], "hash-table")
	}
	ht[args[1]] = value
}
