// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Subsetp{Function: slip.Function{Name: "subsetp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "subsetp",
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
in the _alist_ to return a key for comparison. The same function is also applied to _item_.`,
				},
				{
					Name: "test",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments; the _item_ and each element
in the list at _place_. A return of false will cause _item_ to be prepended.`,
				},
			},
			Return: "object",
			Text: `__subsetp__ returns true if all members of _list-1_ are in _list-2_. If
provided the _:key_ function will be used to derive a key for comparison on each element.
If provide, _:test_ will be used for comparison.`,
			Examples: []string{
				"(subsetp '(1 3 5) '(1 2 3 4 5)) => t",
				"(subsetp '(1 3 5) '(1 2 3 4)) => nil",
			},
		}, &slip.CLPkg)
}

// Subsetp represents the subsetp function.
type Subsetp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Subsetp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 6)
	var (
		list1 slip.List
		list2 slip.List
		ok    bool
		kc    slip.Caller
		tc    slip.Caller
	)

	if list1, ok = args[0].(slip.List); !ok {
		slip.PanicType("list-1", args[0], "list")
	}
	if list2, ok = args[1].(slip.List); !ok {
		slip.PanicType("list-1", args[1], "list")
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
	for _, k1 := range list1 {
		if kc != nil {
			k1 = kc.Call(s, slip.List{k1}, depth)
		}
		var has bool
		for _, k2 := range keys {
			if tc == nil {
				if slip.ObjectEqual(k1, k2) {
					has = true
					break
				}
			} else if tc.Call(s, slip.List{k1, k2}, depth) != nil {
				has = true
				break
			}
		}
		if !has {
			return nil
		}
	}
	return slip.True
}
