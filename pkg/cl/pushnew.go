// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Pushnew{Function: slip.Function{Name: "pushnew", Args: args, SkipEval: []bool{false, true, false}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "pushnew",
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
				{Name: "&key"},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _alist_ to return a key for comparison. The same function is also applied to _item_.`,
				},
				{
					Name: "test",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments; the _item_ and each element
in the list at _place_. A return of false will cause _item_ to be prepended.`,
				},
				{
					Name: "test-not",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments; the _item_ and each element
in the list at _place_. A return of true will cause _item_ to be prepended.`,
				},
			},
			Return: "list",
			Text:   `__pushnew__ returns list referred to by _place_.`,
			Examples: []string{
				"(setq lst '(b c))",
				"(pushnew 'a lst) => (a b c)",
				"(pushnew 'a lst) => (a b c)",
				"lst => (a b c)",
			},
		}, &slip.CLPkg)
}

// Pushnew represents the pushnew function.
type Pushnew struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Pushnew) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 8)
	item := args[0]
	place := args[1]
	var (
		kc slip.Caller
		tc slip.Caller
		nc slip.Caller
	)
	args = args[2:]
	if v, ok := slip.GetArgsKeyValue(args, slip.Symbol(":key")); ok {
		kc = ResolveToCaller(s, v, depth)
	}
	if v, ok := slip.GetArgsKeyValue(args, slip.Symbol(":test")); ok {
		tc = ResolveToCaller(s, v, depth)
	}
	if v, ok := slip.GetArgsKeyValue(args, slip.Symbol(":test-not")); ok {
		nc = ResolveToCaller(s, v, depth)
	}
	d2 := depth + 1
retry:
	switch ta := place.(type) {
	case slip.Symbol:
		switch pv := s.Get(ta).(type) {
		case nil:
			result = slip.List{item}
		case slip.List:
			if itemInList(s, item, pv, kc, tc, nc, d2) {
				result = pv
				break retry
			}
			result = append(slip.List{item}, pv...)
		default:
			slip.TypePanic(s, depth, "place referral", pv, "list")
		}
		s.Set(ta, result)
	case slip.List:
		place = slip.ListToFunc(s, ta, depth+1)
		goto retry
	case slip.Placer:
		switch pv := ta.Eval(s, d2).(type) {
		case nil:
			result = slip.List{item}
		case slip.List:
			if itemInList(s, item, pv, kc, tc, nc, d2) {
				result = pv
				break retry
			}
			result = append(slip.List{item}, pv...)
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

func itemInList(s *slip.Scope, item slip.Object, list slip.List, kc, tc, nc slip.Caller, depth int) bool {
	if kc == nil && tc == nil && nc == nil {
		for _, e := range list {
			if slip.ObjectEqual(item, e) {
				return true
			}
		}
		return false
	}
	if kc != nil {
		item = kc.Call(s, slip.List{item}, depth)
	}
	for _, e := range list {
		if kc != nil {
			e = kc.Call(s, slip.List{e}, depth)
		}
		switch {
		case tc != nil:
			if tc.Call(s, slip.List{item, e}, depth) != nil {
				return true
			}
		case nc != nil:
			if nc.Call(s, slip.List{item, e}, depth) == nil {
				return true
			}
		default:
			if slip.ObjectEqual(item, e) {
				return true
			}
		}
	}
	return false
}
