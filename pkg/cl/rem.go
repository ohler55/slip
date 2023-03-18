// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math"
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Rem{Function: slip.Function{Name: "rem", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "rem",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number to divide by the _divisor_.",
				},
				{
					Name: "divisor",
					Type: "number",
					Text: "The number to divide the _number_ by.",
				},
			},
			Return: "nil",
			Text:   `__rem__ returns the remainder of _number_ truncation by the _divisor_.`,
			Examples: []string{
				"(rem -1 5) => -1",
				"(rem 3.3 1) => 0.3",
			},
		}, &slip.CLPkg)
}

// Rem represents the rem function.
type Rem struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Rem) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	if _, ok := args[1].(slip.Real); !ok {
		slip.PanicType("divisor", args[1], "real")
	}
	n, d := normalizeNumber(args[0], args[1])
	switch num := n.(type) {
	case slip.Fixnum:
		div := int64(d.(slip.Fixnum))
		m := int64(num) % div
		result = slip.Fixnum(m)
	case *slip.Bignum:
		div := (*big.Int)(d.(*slip.Bignum))
		var z big.Int
		_ = z.Rem((*big.Int)(num), div)
		result = (*slip.Bignum)(&z)
	case slip.Real:
		div := (d.(slip.Real)).RealValue()
		nf := num.RealValue()
		m := math.Remainder(nf, div)
		result = slip.DoubleFloat(m)
	case slip.Complex:
		slip.PanicType("number", num, "real")
	}
	return
}
