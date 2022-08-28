// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Ffloor{Function: slip.Function{Name: "ffloor", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "ffloor",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "real",
					Text: `The number to take the quotient of and then rounded
to the next integer towards negative infinity.`,
				},
				{Name: "&optional"},
				{
					Name: "divisor",
					Type: "real",
					Text: `The number to divide the _number_ by. The default is 1.`,
				},
			},
			Return: "integer,real",
			Text: `__ffloor__ returns the quotient of the _numbers_ rounded
toward negative infinity as well as the remainder.`,
			Examples: []string{
				"(ffloor 5.4) => 5.0, 0.4",
				"(ffloor 3/2) => 1.0, 1/2",
				"(ffloor 5 2) => 2.0, 1",
			},
		}, &slip.CLPkg)
}

// Ffloor represents the ffloor function.
type Ffloor struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Ffloor) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	values := floor(f, args)
	switch tv := values[1].(type) {
	case slip.Fixnum:
		switch values[0].(type) {
		case slip.SingleFloat:
			values[1] = slip.SingleFloat(tv)
		default:
			values[1] = slip.DoubleFloat(tv)
		}
	case *slip.Bignum:
		if (*big.Int)(tv).IsInt64() {
			if _, ok := values[0].(*slip.LongFloat); ok {
				var z big.Float
				values[1] = (*slip.LongFloat)(z.SetInt((*big.Int)(tv)))
			} else {
				values[1] = slip.DoubleFloat((*big.Int)(tv).Int64())
			}
		} else {
			var z big.Float
			values[1] = (*slip.LongFloat)(z.SetInt((*big.Int)(tv)))
		}
	}
	return values
}
