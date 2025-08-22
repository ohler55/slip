// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Sxhash{Function: slip.Function{Name: "sxhash", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "sxhash",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The _object_ to return a hash for.",
				},
			},
			Return: "fixnum",
			Text: `__sxhash__ returns a hash code form the _object_ such that two objects
that are _equal_ will have the same has code.`,
			Examples: []string{
				"(sxhash 'abc) => 198",
				"(sxhash 'aBc) => 198",
			},
		}, &slip.CLPkg)
}

// Sxhash represents the sxhash function.
type Sxhash struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Sxhash) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	var h uint64
	for _, b := range sen.Bytes(slip.SimpleObject(args[0])) {
		h += uint64(0xdf & b) // mask 0x20 to ignore ascii case, for others it doesn't matter
	}
	return slip.Fixnum(h & 0x7fffffffffffffff)
}
