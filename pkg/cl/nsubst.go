// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Nsubst{Function: slip.Function{Name: "nsubst", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "nsubst",
			Args: []*slip.DocArg{
				{
					Name: "new",
					Type: "object",
					Text: "The replacement value.",
				},
				{
					Name: "old",
					Type: "object",
					Text: "The value to replace.",
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
			Text: `__nsubst__ returns _tree_ with all occurrences of the _old_ with _new_.
The _tree_ is modified if a replacement is made.`,
			Examples: []string{
				"(setq lst '(a (b (c)))",
				"(nsubst 'b 2 lst) => (a (2 (c))))",
				"lst => (a (2 (c)))",
			},
		}, &slip.CLPkg)
}

// Nsubst represents the nsubst function.
type Nsubst struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Nsubst) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 3, 7)
	var (
		kc slip.Caller
		tc slip.Caller
	)
	kargs := args[3:]
	if v, ok := slip.GetArgsKeyValue(kargs, slip.Symbol(":key")); ok {
		kc = ResolveToCaller(s, v, depth)
	}
	if v, ok := slip.GetArgsKeyValue(kargs, slip.Symbol(":test")); ok {
		tc = ResolveToCaller(s, v, depth)
	}
	return nsubstTree(s, args[2], args[0], args[1], kc, tc, depth+1)
}

func nsubstTree(s *slip.Scope, tree, rep, old slip.Object, kc, tc slip.Caller, depth int) slip.Object {
	key := tree
	if kc != nil {
		key = kc.Call(s, slip.List{tree}, depth)
	}
	if tc != nil {
		if tc.Call(s, slip.List{old, key}, depth) != nil {
			return rep
		}
	} else if slip.ObjectEqual(old, key) {
		return rep
	}
	if list, ok := tree.(slip.List); ok {
		for i, e := range list {
			if tail, ok2 := e.(slip.Tail); ok2 {
				list[i] = slip.Tail{Value: nsubstTree(s, tail.Value, rep, old, kc, tc, depth)}
			} else {
				list[i] = nsubstTree(s, e, rep, old, kc, tc, depth)
			}
		}
	}
	return tree
}
