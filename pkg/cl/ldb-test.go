// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := LdbTest{Function: slip.Function{Name: "ldb-test", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "ldb-test",
			Args: []*slip.DocArg{
				{
					Name: "bytespec",
					Type: "cons",
				},
				{
					Name: "integer",
					Type: "integer",
				},
			},
			Return: "boolean",
			Text: `__ldb-test__ returns the true if any of the bit specified by _bytespec_ in
_integer_ are non-zero, otherwise _nil_ is returned.`,
			Examples: []string{
				"(ldb-test (byte 4 1) 16) => t",
				"(ldb-test (byte 3 1) 16) => nil",
			},
		}, &slip.CLPkg)
}

// LdbTest represents the ldb-test function.
type LdbTest struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *LdbTest) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	// Helper functions are defined in deposit-field.go.
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	integer, _ := ToUnsignedByte(s, args[1], "integer", depth)
	size, pos := byteSpecArg(s, args[0], depth)

	for i := uint(0); i < uint(size); i++ {
		if integer.GetBit(i + uint(pos)) {
			return slip.True
		}
	}
	return nil
}
