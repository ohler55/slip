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
			f := Ceiling{Function: slip.Function{Name: "ceiling", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "ceiling",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "real",
					Text: `The number to take the quotient of and then rounded
to the next integer towards positive infinity.`,
				},
			},
			Return: "real,real",
			Text: `__ceiling__ returns the quotient of the _numbers_ rounded
toward positive infinity as well as the remainder.`,
			Examples: []string{
				"(ceiling 5.4) => 5, 0.4",
				"(ceiling 3/2) => 2.0, -1/2",
				"(ceiling 5 2) => 2, 1",
			},
		}, &slip.CLPkg)
}

// Ceiling represents the ceiling function.
type Ceiling struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Ceiling) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) < 1 || 2 < len(args) {
		slip.PanicArgCount(f, 1, 2)
	}
	num := args[len(args)-1]
	if _, ok := num.(slip.Number); !ok {
		slip.PanicType("number", num, "real")
	}
	var (
		div slip.Object = slip.Fixnum(1)
		q   slip.Object
		r   slip.Object
	)
	if 1 < len(args) {
		div = args[0]
	}
	num, div = normalizeNumber(num, div)

	switch tn := num.(type) {
	case slip.Fixnum:
		q = slip.Fixnum(tn / div.(slip.Fixnum))
		prod := q.(slip.Fixnum) * div.(slip.Fixnum)
		r = slip.Fixnum(tn - prod)
		if 0 < r.(slip.Fixnum) {
			q = q.(slip.Fixnum) + slip.Fixnum(1)
			r = r.(slip.Fixnum) - div.(slip.Fixnum)
		}
	case slip.SingleFloat:
		q = slip.SingleFloat(tn / div.(slip.SingleFloat))
		q = slip.SingleFloat(math.Ceil(float64(q.(slip.SingleFloat))))
		r = slip.SingleFloat(tn - q.(slip.SingleFloat)*div.(slip.SingleFloat))
	case slip.DoubleFloat:
		q = slip.DoubleFloat(tn / div.(slip.DoubleFloat))
		q = slip.DoubleFloat(math.Ceil(float64(q.(slip.DoubleFloat))))
		r = slip.DoubleFloat(tn - q.(slip.DoubleFloat)*div.(slip.DoubleFloat))
	case *slip.LongFloat:
		syncFloatPrec(tn, div.(*slip.LongFloat))
		q = (*slip.LongFloat)((*big.Float)(tn).Quo((*big.Float)(tn), (*big.Float)(div.(*slip.LongFloat))))
		// TBD r

	case *slip.Bignum:
		/*
			var z big.Int
			var zz big.Int
			q, r := zz.QuoRem((*big.Int)(quot.(*slip.Bignum)), (*big.Int)(ta), &z)
			if r.Sign() == 0 {
				quot = (*slip.Bignum)(q)
			} else {
				var zr big.Rat
				quot = (*slip.Ratio)(zr.SetFrac((*big.Int)(quot.(*slip.Bignum)), (*big.Int)(ta)))
			}
		*/
	case *slip.Ratio:
		// quot = (*slip.Ratio)(((*big.Rat)(quot.(*slip.Ratio))).Quo((*big.Rat)(quot.(*slip.Ratio)), (*big.Rat)(ta)))
	case slip.Complex:
		slip.PanicType("number", tn, "real")
	}
	return slip.Values{r, q}
}
