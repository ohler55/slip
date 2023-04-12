// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Decf{Function: slip.Function{Name: "decf", Args: args, SkipEval: []bool{true, false}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "decf",
			Args: []*slip.DocArg{
				{
					Name: "place",
					Type: "placer",
					Text: "The place to decrement.",
				},
				{
					Name: "delta-form",
					Type: "object",
					Text: "The delta amount to decrement the _place_ value.",
				},
			},
			Return: "number",
			Text:   `__decf__ decrements the value in _place_.`,
			Examples: []string{
				"(let ((x 0)) (decf x)) => -1",
				"(let ((x 0)) (decf x 1.5)) => -1.5",
				"(let ((x (list 1 2))) (decf (car x)) x) => (0 2)",
			},
		}, &slip.CLPkg)
}

// Decf represents the decf function.
type Decf struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Decf) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 2)
	var delta slip.Object = slip.Fixnum(-1)
	if 1 < len(args) {
		delta = args[1]
		switch td := delta.(type) {
		case slip.Fixnum:
			delta = -td
		case slip.SingleFloat:
			delta = -td
		case slip.DoubleFloat:
			delta = -td
		case *slip.LongFloat:
			z := (*big.Float)(td)
			result = (*slip.LongFloat)(z.Neg(z))
		case *slip.Bignum:
			z := (*big.Int)(td)
			result = (*slip.Bignum)(z.Neg(z))
		case *slip.Ratio:
			den := (*big.Rat)(td).Denom()
			num := (*big.Rat)(td).Num()
			num = (num.Neg(num))
			delta = (*slip.Ratio)((*big.Rat)(td).SetFrac(num, den))
		case slip.Complex:
			delta = slip.Complex(complex(-real(td), -imag(td)))
		default:
			slip.PanicType("decf value", td, "number")
		}
	}
	d2 := depth + 1
	p := args[0]
Retry:
	switch tp := p.(type) {
	case slip.Symbol:
		result = addNumbers(s.Get(tp), delta)
		s.Set(tp, result)
	case slip.List:
		p = slip.ListToFunc(s, tp, d2)
		goto Retry
	case slip.Placer:
		targs := tp.GetArgs()
		pargs := make(slip.List, len(targs))
		for j, v := range targs {
			if list, ok := v.(slip.List); ok {
				v = slip.ListToFunc(s, list, d2)
			}
			pargs[j] = s.Eval(v, d2)
		}
		result = addNumbers(tp.Apply(s, pargs, d2), delta)
		tp.Place(pargs, result)
	default:
		slip.PanicType("decf placer", tp, "placer", "symbol")
	}
	return
}
