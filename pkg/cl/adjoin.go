// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Adjoin{Function: slip.Function{Name: "adjoin", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "adjoin",
			Args: []*slip.DocArg{
				{
					Name: "item",
					Type: "object",
					Text: "The value to conditionally add to _list_.",
				},
				{
					Name: "list",
					Type: "list",
					Text: "The list to search for _item_ in and concatenate to.",
				},
				{Name: "&key"},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _list_ to return a key for comparison.`,
				},
				{
					Name: "test",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments and returns a boolean to
indicate a match. The default is _equal_`,
				},
			},
			Return: "list",
			Text:   `__adjoin__ adds _item_ if it is not already on _list_.`,
			Examples: []string{
				"(adjoin 'a '(b c)) => (a b c)",
			},
		}, &slip.CLPkg)
}

// Adjoin represents the adjoin function.
type Adjoin struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Adjoin) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 6)
	item := args[0]
	list, ok := args[1].(slip.List)
	if !ok {
		if args[1] != nil {
			slip.PanicType("list", args[1], "list")
		}
	}
	var (
		keyFunc  slip.Caller
		testFunc slip.Caller
	)
	for pos := 2; pos < len(args)-1; pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		keyword := strings.ToLower(string(sym))
		switch keyword {
		case ":key":
			keyFunc = resolveToCaller(s, args[pos+1], depth)
		case ":test":
			testFunc = resolveToCaller(s, args[pos+1], depth)
		default:
			slip.PanicType("keyword", sym, ":key", ":test")
		}
	}
	d2 := depth + 1
	if keyFunc != nil {
		item = keyFunc.Call(s, slip.List{item}, d2)
	}
	for _, v := range list {
		if keyFunc != nil {
			v = keyFunc.Call(s, slip.List{v}, d2)
		}
		if testFunc == nil {
			if slip.ObjectEqual(item, v) {
				// Already on list so return the original list.
				return list
			}
		} else if testFunc.Call(s, slip.List{v, item}, d2) != nil {
			// Already on list so return the original list.
			return list
		}
	}
	return append(slip.List{args[0]}, list...)
}
