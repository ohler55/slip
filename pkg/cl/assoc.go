// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Assoc{Function: slip.Function{Name: "assoc", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "assoc",
			Args: []*slip.DocArg{
				{
					Name: "item",
					Type: "object",
					Text: "The value to match against the _car_ of each element of _alist_.",
				},
				{
					Name: "alist",
					Type: "list",
					Text: "The association list to search for _item_ in.",
				},
				{Name: "&key"},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _alist_ to return a key for comparison.`,
				},
				{
					Name: "test",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments and returns a boolean to
indicate a match. The default is _equal_`,
				},
			},
			Return: "cons|list",
			Text:   `__assoc__ returns the first _cons_ whose _car_ satisfies _test_ or _nil_ if there is no match.`,
			Examples: []string{
				"(assoc 'x '((x . 1) (y. 2) (z . 3))) => (x . 1)",
			},
		}, &slip.CLPkg)
}

// Assoc represents the assoc function.
type Assoc struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Assoc) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 4)
	pos := len(args) - 1
	item := args[pos]
	pos--
	alist, ok := args[pos].(slip.List)
	if !ok {
		slip.PanicType("alist", args[pos], "list")
	}
	pos--
	var (
		keyFunc  slip.Caller
		testFunc slip.Caller
	)
	for ; 0 < pos; pos -= 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		keyword := strings.ToLower(string(sym))
		switch keyword {
		case ":key":
			keyFunc = resolveToCaller(s, args[pos-1], depth)
		case ":test":
			testFunc = resolveToCaller(s, args[pos-1], depth)
		default:
			slip.PanicType("keyword", sym, ":key", ":test")
		}
	}
	switch {
	case testFunc != nil:
		d2 := depth + 1
		var k slip.Object
		for i := len(alist) - 1; 0 <= i; i-- {
			switch tv := alist[i].(type) {
			case nil:
				continue
			case slip.Cons:
				k = tv.Car()
			case slip.List:
				k = tv[len(tv)-1]
			default:
				slip.PanicType("assoc list element", tv, "cons", "list")
			}
			if keyFunc != nil {
				k = keyFunc.Call(s, slip.List{k}, d2)
			}
			if testFunc.Call(s, slip.List{item, k}, d2) != nil {
				return alist[i]
			}
		}
	case keyFunc != nil:
		d2 := depth + 1
		var k slip.Object
		for i := len(alist) - 1; 0 <= i; i-- {
			switch tv := alist[i].(type) {
			case nil:
				continue
			case slip.Cons:
				k = tv.Car()
			case slip.List:
				k = tv[len(tv)-1]
			default:
				slip.PanicType("assoc list element", tv, "cons", "list")
			}
			if slip.ObjectEqual(item, keyFunc.Call(s, slip.List{k}, d2)) {
				return alist[i]
			}
		}
	default:
		var k slip.Object
		for i := len(alist) - 1; 0 <= i; i-- {
			switch tv := alist[i].(type) {
			case nil:
				continue
			case slip.Cons:
				k = tv.Car()
			case slip.List:
				k = tv[len(tv)-1]
			default:
				slip.PanicType("assoc list element", tv, "cons", "list")
			}
			if slip.ObjectEqual(item, k) {
				return alist[i]
			}
		}
	}
	return nil
}
