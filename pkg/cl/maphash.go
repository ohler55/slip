// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Maphash{Function: slip.Function{Name: "maphash", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "maphash",
			Args: []*slip.DocArg{
				{
					Name: "function",
					Type: "symbol|lambda",
					Text: "The function to call for each entry in _hash-table_.",
				},
				{
					Name: "hash-table",
					Type: "hash-table",
					Text: "The _hash-table_ to iterate over.",
				},
			},
			Return: "nil",
			Text: `__maphash__ calls _function_ for each entry in the _hash-table_
with two arguments, the key and the value in that order.`,
			Examples: []string{
				"(setq table (make-hash-table))",
				"(setf (maphash 'a table) 1) => 1",
				`(maphash (lambda (k v) (format t "~A ~A~%" k v)) table) => nil ;; prints "a 1"`,
			},
		}, &slip.CLPkg)
}

// Maphash represents the maphash function.
type Maphash struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Maphash) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	ht, ok := args[1].(slip.HashTable)
	if !ok {
		slip.PanicType("hash-table", args[0], "hash-table")
	}
	fn := args[0]
	d2 := depth + 1
	caller := resolveToCaller(s, fn, d2)
	for k, v := range ht {
		_ = caller.Call(s, slip.List{v, k}, d2)
	}
	return nil
}
