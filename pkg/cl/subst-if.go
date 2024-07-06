// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SubstIf{Function: slip.Function{Name: "subst-if", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "subst-if",
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
			Text: `__subst-if__ returns _tree_ with all occurrences of the values
where _predicate_ returns true with _new_.`,
			Examples: []string{
				"(setq lst '(a (b (c)))",
				"(subst-if 2 (lambda (v) (equal v 'b)) lst) => (a (2 (c))))",
				"lst => (a (b (c)))",
			},
		}, &slip.CLPkg)
}

// SubstIf represents the subst-if function.
type SubstIf struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SubstIf) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 3, 7)
	pc := ResolveToCaller(s, args[1], depth)
	var kc slip.Caller
	if v, ok := slip.GetArgsKeyValue(args[3:], slip.Symbol(":key")); ok {
		kc = ResolveToCaller(s, v, depth)
	}
	return substIfTree(s, args[2], args[0], pc, kc, depth+1)
}

func substIfTree(s *slip.Scope, tree, rep slip.Object, pc, kc slip.Caller, depth int) slip.Object {
	key := tree
	if kc != nil {
		key = kc.Call(s, slip.List{tree}, depth)
	}
	if pc.Call(s, slip.List{key}, depth) != nil {
		return rep
	}
	if list, ok := tree.(slip.List); ok {
		dup := make(slip.List, len(list))
		for i, e := range list {
			if tail, ok2 := e.(slip.Tail); ok2 {
				dup[i] = slip.Tail{Value: substIfTree(s, tail.Value, rep, pc, kc, depth)}
			} else {
				dup[i] = substIfTree(s, e, rep, pc, kc, depth)
			}
		}
		return dup
	}
	return tree
}
