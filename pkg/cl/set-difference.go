// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SetDifference{Function: slip.Function{Name: "set-difference", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "set-difference",
			Args: []*slip.DocArg{
				{
					Name: "list-1",
					Type: "list",
				},
				{
					Name: "list-2",
					Type: "list",
				},
				{Name: "&key"},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the lists to return a key for comparison..`,
				},
				{
					Name: "test",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments; the _item_ and each element
in the list at _place_. A return of true indicates a match.`,
				},
			},
			Return: "sequence",
			Text:   `__set-difference__ returns a list of all element from _list-1_ not in _list-2_.`,
			Examples: []string{
				"(set-difference '(a b c) '(b d)) => (a c)",
			},
		}, &slip.CLPkg)
}

// SetDifference represents the set-difference function.
type SetDifference struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SetDifference) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 6)
	var (
		list1 slip.List
		list2 slip.List
		kc    slip.Caller
		tc    slip.Caller
		ok    bool
	)
	if list1, ok = args[0].(slip.List); !ok && args[0] != nil {
		slip.TypePanic(s, depth, "list-1", args[0], "list")
	}
	if list2, ok = args[1].(slip.List); !ok && args[1] != nil {
		slip.TypePanic(s, depth, "list-2", args[1], "list")
	}
	args = args[2:]
	if v, ok := slip.GetArgsKeyValue(args, slip.Symbol(":key")); ok {
		kc = ResolveToCaller(s, v, depth)
	}
	if v, ok := slip.GetArgsKeyValue(args, slip.Symbol(":test")); ok {
		tc = ResolveToCaller(s, v, depth)
	}
	keys := list2
	if kc != nil {
		keys = make(slip.List, len(list2))
		for i, v := range list2 {
			keys[i] = kc.Call(s, slip.List{v}, depth)
		}
	}
	var diff slip.List
	for _, v1 := range list1 {
		k1 := v1
		if kc != nil {
			k1 = kc.Call(s, slip.List{v1}, depth)
		}
		var has bool
		for _, k2 := range keys {
			if tc != nil {
				if tc.Call(s, slip.List{k1, k2}, depth) != nil {
					has = true
					break
				}
			} else if slip.ObjectEqual(k1, k2) {
				has = true
				break
			}
		}
		if !has {
			diff = append(diff, v1)
		}
	}
	return diff
}
