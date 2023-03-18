// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Fceiling{Function: slip.Function{Name: "fceiling", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "fceiling",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "real",
					Text: `The number to take the quotient of and then rounded
to the next integer towards positive infinity.`,
				},
				{Name: "&optional"},
				{
					Name: "divisor",
					Type: "real",
					Text: `The number to divide the _number_ by. The default is 1.`,
				},
			},
			Return: "float,real",
			Text: `__fceiling__ returns the quotient of the _numbers_ rounded
toward positive infinity as well as the remainder.`,
			Examples: []string{
				"(fceiling 5.4) => 6.0, -0.6",
				"(fceiling 3/2) => 2.0, -1/2",
				"(fceiling 5 2) => 3.0, -1",
			},
		}, &slip.CLPkg)
}

// Fceiling represents the fceiling function.
type Fceiling struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Fceiling) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	values := ceiling(f, args)
	switch tv := values[0].(type) {
	case slip.Fixnum:
		switch values[1].(type) {
		case slip.SingleFloat:
			values[0] = slip.SingleFloat(tv)
		default:
			values[0] = slip.DoubleFloat(tv)
		}
	case *slip.Bignum:
		if (*big.Int)(tv).IsInt64() {
			if _, ok := values[1].(*slip.LongFloat); ok {
				var z big.Float
				values[0] = (*slip.LongFloat)(z.SetInt((*big.Int)(tv)))
			} else {
				values[0] = slip.DoubleFloat((*big.Int)(tv).Int64())
			}
		} else {
			var z big.Float
			values[0] = (*slip.LongFloat)(z.SetInt((*big.Int)(tv)))
		}
	}
	return values
}
