// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Intersection{Function: slip.Function{Name: "intersection", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "intersection",
			Args: []*slip.DocArg{
				{
					Name: "list-1",
					Type: "list",
					Text: `The first _list_ for the intersection.`,
				},
				{
					Name: "list-2",
					Type: "list",
					Text: `The second _list_ for the intersection.`,
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
			Text:   `__intersection__ returns a list of all the elements present in both lists. Order is arbitrary.`,
			Examples: []string{
				`(intersection '(a b c b a) '(d a b) => (a b)`,
			},
		}, &slip.CLPkg)
}

// Intersection represents the intersection function.
type Intersection struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Intersection) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	lists, keyFunc, testFunc := list2TestKeyArgs(s, f, args, depth)
	if len(lists) != 2 {
		return nil
	}
	var rlist slip.List
	if testFunc == nil && keyFunc == nil {
		list2 := lists[1]
		for _, obj1 := range lists[0] {
			if objInList(obj1, rlist) {
				continue
			}
			for _, obj2 := range list2 {
				if slip.ObjectEqual(obj1, obj2) {
					rlist = append(rlist, obj1)
					break
				}
			}
		}
	} else {
		var keys slip.List
		d2 := depth + 1
		list2 := lists[1]
		keys2 := make(slip.List, len(list2))
		if keyFunc == nil {
			copy(keys2, list2)
		} else {
			for i, obj := range list2 {
				keys2[i] = keyFunc.Call(s, slip.List{obj}, d2)
			}
		}
		for _, obj1 := range lists[0] {
			k1 := obj1
			if keyFunc != nil {
				k1 = keyFunc.Call(s, slip.List{k1}, d2)
			}
			if testFunc == nil {
				if objInList(k1, keys) {
					continue
				}
			} else if objInListTest(s, k1, keys, testFunc, d2) {
				continue
			}
			for _, k2 := range keys2 {
				if testFunc == nil {
					if slip.ObjectEqual(k1, k2) {
						rlist = append(rlist, obj1)
						keys = append(keys, k1)
						break
					}
				} else if testFunc.Call(s, slip.List{k1, k2}, d2) != nil {
					rlist = append(rlist, obj1)
					keys = append(keys, k1)
					break
				}
			}
		}
	}
	return rlist
}
