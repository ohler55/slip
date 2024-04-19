// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Logbitp{Function: slip.Function{Name: "logbitp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "logbitp",
			Args: []*slip.DocArg{
				{
					Name: "index",
					Type: "fixnum",
					Text: "A non-negative fixnum.",
				},
				{
					Name: "integer",
					Type: "integer",
					Text: "An integer.",
				},
			},
			Return: "boolean",
			Text:   `__logbitp__ returns true if the _index_ bit of _integer_ is set.`,
			Examples: []string{
				"(logbitp 2 5) => t",
				"(logbitp 1 5) => nil",
			},
		}, &slip.CLPkg)
}

// Logbitp represents the logbitp function.
type Logbitp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Logbitp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	index, ok := args[0].(slip.Fixnum)
	if !ok || index < 0 {
		slip.PanicType("index", args[0], "non-negative fixnum")
	}
	switch ti := args[1].(type) {
	case slip.Fixnum:
		if int(index) < 64 {
			if (uint64(ti)>>int(index))&0x01 == 1 {
				return slip.True
			}
		}
	case *slip.Bignum:
		ba := (*big.Int)(ti).Bytes()
		reverseBytes(ba)
		bo := int(index) / 8
		if bo < len(ba) {
			if 0 < (*big.Int)(ti).Sign() {
				if (ba[bo]>>(index%8))&0x01 == 1 {
					return slip.True
				}
			} else {
				if (ba[bo]>>(index%8))&0x01 != 1 {
					return slip.True
				}
			}
		}
	default:
		slip.PanicType("integer", ti, "integer")
	}
	return nil
}
