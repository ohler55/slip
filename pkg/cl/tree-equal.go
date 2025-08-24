// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TreeEqual{Function: slip.Function{Name: "tree-equal", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "tree-equal",
			Args: []*slip.DocArg{
				{
					Name: "tree-1",
					Type: "object",
					Text: `A tree to compare.`,
				},
				{
					Name: "tree-2",
					Type: "object",
					Text: `A second tree to compare.`,
				},
				{Name: "&key"},
				{
					Name: "test",
					Type: "symbol|function",
					Text: `The test to use instead of the default _equal_.`,
				},
			},
			Return: "boolean",
			Text: `__tree-equal__ returns _t_ if _tree-1_ _car_ equals _tree-2_ _car_ and
_tree-1_ _cdr_ equals _tree-2_ _cdr_.`,
			Examples: []string{
				`(tree-equal '(a (b c)) '(a (b c))) => t`,
			},
		}, &slip.CLPkg)
}

// TreeEqual represents the tree-equal function.
type TreeEqual struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TreeEqual) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 4)
	if 2 < len(args) {
		var testFunc slip.Caller
		for pos := 2; pos < len(args); pos += 2 {
			sym, ok := args[pos].(slip.Symbol)
			if !ok {
				slip.TypePanic(s, depth, "keyword", args[pos], "keyword")
			}
			if len(args)-1 <= pos {
				slip.NewPanic("%s missing an argument", sym)
			}
			if strings.EqualFold(string(sym), ":test") {
				testFunc = ResolveToCaller(s, args[pos+1], depth)
			} else {
				slip.TypePanic(s, depth, "keyword", sym, ":test")
			}
		}
		if testFunc.Call(s, args[:2], depth+1) != nil {
			return slip.True
		}
		return nil
	}
	if slip.ObjectEqual(args[0], args[1]) {
		return slip.True
	}
	return nil
}
