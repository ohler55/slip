// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Rassoc{Function: slip.Function{Name: "rassoc", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "rassoc",
			Args: []*slip.DocArg{
				{
					Name: "item",
					Type: "object",
					Text: "The value to match against the _cdr_ of each element of _alist_.",
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
			Return: "cons",
			Text:   `__rassoc__ returns the first _cons_ whose _cdr_ satisfies _test_ or _nil_ if there is no match.`,
			Examples: []string{
				"(rassoc 1 '((x . 1) (y. 2) (z . 3))) => (x . 1)",
			},
		}, &slip.CLPkg)
}

// Rassoc represents the rassoc function.
type Rassoc struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Rassoc) Call(s *slip.Scope, args slip.List, depth int) (found slip.Object) {
	slip.ArgCountCheck(f, args, 2, 6)
	pos := 0
	item := args[pos]
	pos++
	alist, ok := args[pos].(slip.List)
	if !ok {
		slip.PanicType("alist", args[pos], "list")
	}
	pos++
	var (
		keyFunc  slip.Caller
		testFunc slip.Caller
	)
	for ; pos < len(args)-1; pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		keyword := strings.ToLower(string(sym))
		switch keyword {
		case ":key":
			keyFunc = ResolveToCaller(s, args[pos+1], depth)
		case ":test":
			testFunc = ResolveToCaller(s, args[pos+1], depth)
		default:
			slip.PanicType("keyword", sym, ":key", ":test")
		}
	}
	d2 := depth + 1
	var k slip.Object
	for _, a := range alist {
		switch tv := a.(type) {
		case nil:
			continue
		case slip.List:
			k = tv.Cdr()
		default:
			slip.PanicType("rassoc list element", tv, "cons")
		}
		if keyFunc != nil {
			k = keyFunc.Call(s, slip.List{k}, d2)
		}
		if testFunc == nil {
			if slip.ObjectEqual(item, k) {
				found = a
				break
			}
		} else if testFunc.Call(s, slip.List{k, item}, d2) != nil {
			found = a
			break
		}
	}
	return
}
