// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Push{Function: slip.Function{Name: "push", Args: args, SkipEval: []bool{false, true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "push",
			Args: []*slip.DocArg{
				{
					Name: "item",
					Type: "object",
					Text: "The item to push onto the list.",
				},
				{
					Name: "place",
					Type: "list placer",
					Text: "The placer that references a list.",
				},
			},
			Return: "object",
			Text:   `__push__ returns the list referenced by _place_ after pushing _item_ to the head of the list.`,
			Examples: []string{
				"(setq lst '(b c))",
				"(push 'a lst) => (a b c)",
				"lst => (a b c)",
			},
		}, &slip.CLPkg)
}

// Push represents the push function.
type Push struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Push) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 2)

	place := args[1]
retry:
	switch ta := place.(type) {
	case slip.Symbol:
		switch pv := s.Get(ta).(type) {
		case nil:
			result = slip.List{args[0]}
		case slip.List:
			result = append(slip.List{args[0]}, pv...)
		default:
			slip.TypePanic(s, depth, "place referral", pv, "list")
		}
		s.Set(ta, result)
	case slip.List:
		place = slip.ListToFunc(s, ta, depth+1)
		goto retry
	case slip.Placer:
		d2 := depth + 1
		switch pv := ta.Eval(s, d2).(type) {
		case nil:
			result = slip.List{args[0]}
		case slip.List:
			result = append(slip.List{args[0]}, pv...)
		default:
			slip.TypePanic(s, depth, "place referral", pv, "list")
		}
		targs := ta.GetArgs()
		pargs := make(slip.List, len(targs))
		for i, v := range targs {
			pargs[i] = s.Eval(v, d2)
		}
		ta.Place(s, pargs, result)
	default:
		slip.TypePanic(s, depth, "place", ta, "symbol", "placer")
	}
	return
}
