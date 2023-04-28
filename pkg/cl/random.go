// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"
	"math/rand"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Random{Function: slip.Function{Name: "random", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "random",
			Args: []*slip.DocArg{
				{
					Name: "limit",
					Type: "real",
					Text: "A positive real number that is the limit of the random value.",
				},
				{Name: "&optional"},
				{
					Name: "state",
					Type: "random-state",
					Text: "A random state.",
				},
			},
			Return: "real",
			Text:   `__random__ returns a random number less than _limit_.`,
			Examples: []string{
				"(random 5) => 3",
				"(random 7.5) => 3.9",
			},
		}, &slip.CLPkg)
}

// Random represents the random function.
type Random struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Random) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 && len(args) != 2 {
		slip.PanicArgCount(f, 1, 2)
	}
	var rs *RandomState
	if 1 < len(args) {
		if args[1] == slip.True {
			rs = NewRandomState(nil)
		} else {
			switch state := args[1].(type) {
			case nil:
				obj, _ := slip.CLPkg.Get(randomStateStr)
				rs, _ = obj.(*RandomState)
			case *RandomState:
				rs = state
			}
		}
	}
	if rs == nil {
		obj, _ := slip.CLPkg.Get(randomStateStr)
		rs, _ = obj.(*RandomState)
	}
	switch limit := args[0].(type) {
	case slip.Fixnum:
		result = slip.Fixnum(rs.Uint64() % uint64(limit))
	case *slip.Bignum:
		var z big.Int
		result = (*slip.Bignum)(z.Rand(rand.New(rs), (*big.Int)(limit)))
	case slip.SingleFloat:
		result = slip.SingleFloat(float64(rs.Uint64()%(1<<53))/float64(1<<53)) * limit
	case slip.DoubleFloat:
		result = slip.DoubleFloat(float64(rs.Uint64()%(1<<53))/float64(1<<53)) * limit
	case *slip.LongFloat:
		var z big.Float
		_ = z.Mul(big.NewFloat(float64(rs.Uint64()%(1<<53))/float64(1<<53)), (*big.Float)(limit))
		result = (*slip.LongFloat)(&z)
	default:
		slip.PanicType("limit", limit, "real")
	}
	return
}
