// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
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
			Return: "nil",
			Text:   `__random__ returns a random numner less than _limit_.`,
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

// Call the the function with the arguments provided.
func (f *Random) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 && len(args) != 2 {
		slip.PanicArgCount(f, 1, 2)
	}
	var rs *RandomState
	if 1 < len(args) {
		if args[0] == slip.True {
			rs = NewRandomState(nil)
		} else {
			switch state := args[0].(type) {
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
	switch limit := args[len(args)-1].(type) {
	case slip.Fixnum:
		result = slip.Fixnum(rs.Uint64() % uint64(limit))
	case *slip.Bignum:
		// TBD
	case slip.SingleFloat:
		result = slip.SingleFloat(float64(rs.Uint64()%(1<<53))/float64(1<<53)) * limit
	case slip.DoubleFloat:
		result = slip.DoubleFloat(float64(rs.Uint64()%(1<<53))/float64(1<<53)) * limit
	case *slip.LongFloat:
		// TBD
	default:
		slip.PanicType("limit", limit, "real")
	}
	return
}
