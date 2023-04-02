// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"strings"

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
				`(union '(a b c b a) '(d a b) => (a c b d)`,
			},
		}, &slip.CLPkg)
}

// Union represents the union function.
type Union struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Union) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 6)
	var (
		lists    []slip.List
		keyFunc  slip.Caller
		testFunc slip.Caller
	)
	switch ta := args[0].(type) {
	case nil:
		// ok
	case slip.List:
		lists = append(lists, ta)
	default:
		slip.PanicType("list-1", ta, "list")
	}
	switch ta := args[1].(type) {
	case nil:
		// ok
	case slip.List:
		lists = append(lists, ta)
	default:
		slip.PanicType("list-2", ta, "list")
	}
	if 2 < len(args) {
		for pos := 2; pos < len(args); pos += 2 {
			sym, ok := args[pos].(slip.Symbol)
			if !ok {
				slip.PanicType("keyword", args[pos], "keyword")
			}
			if len(args)-1 <= pos {
				panic(fmt.Sprintf("%s missing an argument", sym))
			}
			switch strings.ToLower(string(sym)) {
			case ":key":
				keyFunc = resolveToCaller(s, args[pos+1], depth)
			case ":test":
				testFunc = resolveToCaller(s, args[pos+1], depth)
			default:
				slip.PanicType("keyword", sym, ":key", ":test")
			}
		}
	}
	var rlist slip.List
	if testFunc == nil && keyFunc == nil {
		for _, list := range lists {
			for _, obj := range list {
				found := false
				for _, x := range rlist {
					if slip.ObjectEqual(x, obj) {
						found = true
						break
					}
				}
				if !found {
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
