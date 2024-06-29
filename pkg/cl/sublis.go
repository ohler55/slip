// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Sublis{Function: slip.Function{Name: "sublis", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "sublis",
			Args: []*slip.DocArg{
				{
					Name: "alist",
					Type: "association list",
					Text: "The substitution pairs.",
				},
				{
					Name: "tree",
					Type: "object",
					Text: "The data to replace values in.",
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
			Text:   `__sublis__ returns _tree_ with all occurrences of the _alist_ cars with _alist_ cdrs.`,
			Examples: []string{
				"(setq lst '(a (b (c)))",
				"(sublis '((a . 1) (b . 2) (c . 3)) lst) => (1 (2 (3))))",
				"lst => (a (b (c)))",
			},
		}, &slip.CLPkg)
}

// Sublis represents the sublis function.
type Sublis struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Sublis) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 6)
	alist, ok := args[0].(slip.List)
	if !ok {
		slip.PanicType("alist", args[0], "association list")
	}
	for _, a := range alist {
		if _, ok = a.(slip.List); !ok {
			slip.PanicType("alist", args[0], "association list")
		}
	}
	tree := args[1]
	var (
		kc slip.Caller
		tc slip.Caller
	)
	args = args[2:]
	if v, ok := slip.GetArgsKeyValue(args, slip.Symbol(":key")); ok {
		kc = ResolveToCaller(s, v, depth)
	}
	if v, ok := slip.GetArgsKeyValue(args, slip.Symbol(":test")); ok {
		tc = ResolveToCaller(s, v, depth)
	}
	return subTree(s, tree, alist, kc, tc, depth+1)
}

func subTree(s *slip.Scope, tree slip.Object, subs slip.List, kc, tc slip.Caller, depth int) slip.Object {
	key := tree
	if kc != nil {
		key = kc.Call(s, slip.List{tree}, depth)
	}
	for _, a := range subs {
		cons := a.(slip.List)
		car := cons.Car()
		if tc != nil {
			if tc.Call(s, slip.List{car, key}, depth) != nil {
				return cons.Cdr()
			}
		} else if slip.ObjectEqual(car, key) {
			return cons.Cdr()
		}
	}
	if list, ok := tree.(slip.List); ok {
		dup := make(slip.List, len(list))
		for i, e := range list {
			if tail, ok2 := e.(slip.Tail); ok2 {
				dup[i] = slip.Tail{Value: subTree(s, tail.Value, subs, kc, tc, depth)}
			} else {
				dup[i] = subTree(s, e, subs, kc, tc, depth)
			}
		}
		return dup
	}
	return tree
}
