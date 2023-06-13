// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Union{Function: slip.Function{Name: "union", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "union",
			Args: []*slip.DocArg{
				{
					Name: "list-1",
					Type: "list",
					Text: `The first _list_ for the union.`,
				},
				{
					Name: "list-2",
					Type: "list",
					Text: `The second _list_ for the union.`,
				},
				{Name: "&key"},
				{
					Name: "test",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments and returns _t_ to
indicate the two argument are equal. The default is __equal__.`,
				},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _list_ to return a key for comparison.`,
				},
			},
			Return: "list",
			Text:   `__union__ returns a list of all the unique elements of both list. Order is arbitrary.`,
			Examples: []string{
				`(union '(a b c b a) '(d a b)) => (a c b d)`,
			},
		}, &slip.CLPkg)
}

// Union represents the union function.
type Union struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Union) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	lists, keyFunc, testFunc := list2TestKeyArgs(s, f, args, depth)
	var rlist slip.List
	if testFunc == nil && keyFunc == nil {
		for _, list := range lists {
			for _, obj := range list {
				if !objInList(obj, rlist) {
					rlist = append(rlist, obj)
				}
			}
		}
	} else {
		var keys slip.List
		d2 := depth + 1
		for _, list := range lists {
			for _, obj := range list {
				found := false
				key := obj
				if keyFunc != nil {
					key = keyFunc.Call(s, slip.List{key}, d2)
				}
				for _, k := range keys {
					if testFunc == nil {
						if slip.ObjectEqual(k, key) {
							found = true
							break
						}
					} else if testFunc.Call(s, slip.List{k, key}, d2) != nil {
						found = true
						break
					}
				}
				if !found {
					keys = append(keys, key)
					rlist = append(rlist, obj)
				}
			}
		}
	}
	return rlist
}
