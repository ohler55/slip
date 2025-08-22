// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Pop{Function: slip.Function{Name: "pop", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "pop",
			Args: []*slip.DocArg{
				{
					Name: "place",
					Type: "list placer",
					Text: "The placer that references a list.",
				},
			},
			Return: "object",
			Text: `__pop__ returns the _car_ of the list referred to by _place_ and sets
the value of the list referred to by _place_ to the _cdr_ of the original.`,
			Examples: []string{
				"(setq list '(a b c))",
				"(pop list) => a",
				"list => (b c)",
			},
		}, &slip.CLPkg)
}

// Pop represents the pop function.
type Pop struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Pop) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)

	place := args[0]
retry:
	switch ta := place.(type) {
	case slip.Symbol:
		switch pv := s.Get(ta).(type) {
		case nil:
			// no change
		case slip.List:
			if 0 < len(pv) {
				result = pv[0]
				s.Set(ta, pv[1:])
			}
		default:
			slip.TypePanic(s, depth, "place referral", pv, "list")
		}
	case slip.List:
		place = slip.ListToFunc(s, ta, depth+1)
		goto retry
	case slip.Placer:
		d2 := depth + 1
		switch pv := ta.Eval(s, d2).(type) {
		case nil:
			// no change
		case slip.List:
			if 0 < len(pv) {
				result = pv[0]
				targs := ta.GetArgs()
				pargs := make(slip.List, len(targs))
				for i, v := range targs {
					pargs[i] = s.Eval(v, d2)
				}
				ta.Place(s, pargs, pv[1:])
			}
		default:
			slip.TypePanic(s, depth, "place referral", pv, "list")
		}
	default:
		slip.TypePanic(s, depth, "place", ta, "symbol", "placer")
	}
	return
}
