// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DecodeFloat{Function: slip.Function{Name: "decode-float", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "decode-float",
			Args: []*slip.DocArg{
				{
					Name: "float",
					Type: "float",
					Text: "A float to decode.",
				},
			},
			Return: "float,fixnum,float",
			Text: `__decode-float__ returns the significand, exponent, and sign as a _float_ of
either 1.0 or -1.0. The _significand_ when multiplied by the sign and the base (2) raised to
the exponent will yield the original _float_ argument within some margin of error.`,
			Examples: []string{
				"(decode-float 0.25) => ",
			},
		}, &slip.CLPkg)
}

// DecodeFloat represents the decode-float function.
type DecodeFloat struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *DecodeFloat) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if r, ok := args[0].(slip.Float); ok {
		rv := r.RealValue()
		neg := slip.DoubleFloat(1.0)
		if rv < 0 {
			neg = slip.DoubleFloat(-1.0)
			rv = -rv
		}
		sig, exp := math.Frexp(rv)
		result = slip.Values{slip.DoubleFloat(sig), slip.Fixnum(exp), neg}
	} else {
		slip.TypePanic(s, depth, "float", args[0], "real")
	}
	return
}
