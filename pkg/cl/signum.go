// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Signum{Function: slip.Function{Name: "signum", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "signum",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number to check.",
				},
			},
			Return: "nil",
			Text:   `__signum__ returns 1 if _number_ is positive, -1 if negative, and 0 if the number is 0.`,
			Examples: []string{
				"(signum 5) => t",
				"(signum -5) => nil",
			},
		}, &slip.CLPkg)
}

// Signum represents the signum function.
type Signum struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Signum) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	sig := 0
	switch ta := args[0].(type) {
	case slip.Fixnum:
		switch {
		case 0 < ta:
			sig = 1
		case ta < 0:
			sig = -1
		}
	case slip.SingleFloat:
		switch {
		case 0.0 < ta:
			sig = 1
		case ta < 0.0:
			sig = -1
		}
	case slip.DoubleFloat:
		switch {
		case 0.0 < ta:
			sig = 1
		case ta < 0.0:
			sig = -1
		}
	case *slip.LongFloat:
		sig = (*big.Float)(ta).Sign()
	case *slip.Bignum:
		sig = (*big.Int)(ta).Sign()
	case *slip.Ratio:
		v := ta.RealValue()
		switch {
		case 0.0 < v:
			sig = 1
		case v < 0.0:
			sig = -1
		}
	case slip.Complex:
		r := real(ta)
		i := imag(ta)

		rs := 0.0
		switch {
		case 0.0 < r:
			rs = 1.0
		case r < 0.0:
			rs = -1.0
		}
		is := 0.0
		switch {
		case 0.0 < i:
			is = 1.0
		case i < 0.0:
			is = -1.0
		}
		return slip.Complex(complex(rs, is))
	default:
		slip.PanicType("number", ta, "number")
	}
	return slip.Fixnum(sig)
}
