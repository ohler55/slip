// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := NsubstIf{Function: slip.Function{Name: "nsubst-if", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "nsubst-if",
			Args: []*slip.DocArg{
				{
					Name: "new",
					Type: "object",
					Text: "The replacement value.",
				},
				{
					Name: "predicate",
					Type: "symbol|lambda",
					Text: "The function to call to determine if a value should be replaced.",
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
			},
			Return: "object",
			Text: `__nsubst-if__ returns _tree_ with all occurrences of the values
where _predicate_ returns true with _new_. The _tree_ is modified if a replacement is made.`,
			Examples: []string{
				"(setq lst '(a (b (c)))",
				"(nsubst-if 2 (lambda (v) (equal v 'b)) lst) => (a (2 (c))))",
				"lst => (a (b (c)))",
			},
		}, &slip.CLPkg)
}

// NsubstIf represents the nsubst-if function.
type NsubstIf struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *NsubstIf) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 3, 7)
	pc := ResolveToCaller(s, args[1], depth)
	var kc slip.Caller
	if v, ok := slip.GetArgsKeyValue(args[3:], slip.Symbol(":key")); ok {
		kc = ResolveToCaller(s, v, depth)
	}
	return nsubstIfTree(s, args[2], args[0], pc, kc, depth+1)
}

func nsubstIfTree(s *slip.Scope, tree, rep slip.Object, pc, kc slip.Caller, depth int) slip.Object {
	key := tree
	if kc != nil {
		key = kc.Call(s, slip.List{tree}, depth)
	}
	if pc.Call(s, slip.List{key}, depth) != nil {
		return rep
	}
	if list, ok := tree.(slip.List); ok {
		for i, e := range list {
			if tail, ok2 := e.(slip.Tail); ok2 {
				list[i] = slip.Tail{Value: nsubstIfTree(s, tail.Value, rep, pc, kc, depth)}
			} else {
				list[i] = nsubstIfTree(s, e, rep, pc, kc, depth)
			}
		}
	}
	return tree
}
