// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeHashTable{Function: slip.Function{Name: "make-hash-table", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-hash-table",
			Args: []*slip.DocArg{
				{Name: "&key"},
				{
					Name: "test",
					Type: "symbol",
					Text: "The function to use for comparison of keys. Ignore and __eql__ always used.",
				},
				{Name: "size", Type: "integer", Text: "Ignored."},
				{Name: "rehash-size", Type: "real", Text: "Ignored."},
				{Name: "rehash-threshold", Type: "real", Text: "Ignored."},
			},
			Return: "nil",
			Text:   `__make-hash-table__ returns a new _hash-table_.`,
			Examples: []string{
				"(make-hash-table) => #<hash-table eql 0/--)>",
			},
		}, &slip.CLPkg)
}

// MakeHashTable represents the makeHashTable function.
type MakeHashTable struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeHashTable) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if 1 < len(args) {
		slip.PanicArgCount(f, 0, 8)
	}
	return slip.HashTable{}
}
