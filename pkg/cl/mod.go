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
			f := Mod{Function: slip.Function{Name: "mod", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "mod",
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
			Text:   `__mod__ returns the modulus of _number_ and _divisor_.`,
			Examples: []string{
				"(mod -1 5) => 4",
				"(mod 3.3 1) => 0.3",
			},
		}, &slip.CLPkg)
}

// Mod represents the mod function.
type Mod struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Mod) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	if _, ok := args[1].(slip.Real); !ok {
		slip.PanicType("divisor", args[1], "real")
	}
	n, d := slip.NormalizeNumber(args[0], args[1])
	switch num := n.(type) {
	case slip.Fixnum:
		div := int64(d.(slip.Fixnum))
		if div == 0 {
			slip.PanicArithmetic(slip.Symbol("/"), args, "divide by zero")
		}
		m := int64(num) % div
		if (0 < div && m < 0) || (div < 0 && 0 < m) {
			m += div
		}
		result = slip.Fixnum(m)
	case *slip.Bignum:
		div := (*big.Int)(d.(*slip.Bignum))
		if div.Sign() == 0 {
			slip.PanicArithmetic(slip.Symbol("/"), args, "divide by zero")
		}
		var z big.Int
		_ = z.Mod((*big.Int)(num), div)
		zs := z.Sign()
		ds := div.Sign()
		if (0 < ds && zs < 0) || (ds < 0 && 0 < zs) {
			_ = z.Add(&z, div)
		}
		result = (*slip.Bignum)(&z)
	case slip.Real:
		div := (d.(slip.Real)).RealValue()
		if div == 0.0 {
			slip.PanicArithmetic(slip.Symbol("/"), args, "divide by zero")
		}
		nf := num.RealValue()
		m := math.Mod(nf, div)
		if (0.0 < div && m < 0) || (div < 0 && 0 < m) {
			m += div
		}
		result = slip.DoubleFloat(m)
	case slip.Complex:
		slip.PanicType("number", num, "real")
	}
	return
}
