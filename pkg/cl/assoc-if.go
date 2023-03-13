// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := AssocIf{Function: slip.Function{Name: "assoc-if", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "assoc-if",
			Args: []*slip.DocArg{
				{
					Name: "predicate",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to the car of each element
in the _alist_. If non-nil is returned then the associated _cons_ is return.`,
				},
				{
					Name: "alist",
					Type: "list",
					Text: "The association list to search.",
				},
				{Name: "&key"},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _alist_ to return a key for comparison.`,
				},
			},
			Return: "cons|list",
			Text: `__assoc-if__ returns the first _cons_ whose _car_ satisfies _predicate_ or _nil_
if there is no match.`,
			Examples: []string{
				"(assoc-if (lambda (k) (equal 'x k)) '((x . 1) (y. 2) (z . 3))) => (x . 1)",
			},
		}, &slip.CLPkg)
}

// AssocIf represents the assocIf function.
type AssocIf struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *AssocIf) Call(s *slip.Scope, args slip.List, depth int) (found slip.Object) {
	slip.ArgCountCheck(f, args, 2, 4)
	pos := len(args) - 1
	predicate := resolveToCaller(s, args[pos], depth)
	pos--
	alist, ok := args[pos].(slip.List)
	if !ok {
		slip.PanicType("alist", args[pos], "list")
	}
	pos--
	var keyFunc slip.Caller
	for ; 0 < pos; pos -= 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		if keyword := strings.ToLower(string(sym)); keyword == ":key" {
			keyFunc = resolveToCaller(s, args[pos-1], depth)
		} else {
			slip.PanicType("keyword", sym, ":key")
		}
	}
	d2 := depth + 1
	var k slip.Object
	for i := len(alist) - 1; 0 <= i; i-- {
		switch tv := alist[i].(type) {
		case nil:
			continue
		case slip.List:
			k = tv.Car()
		default:
			slip.PanicType("assoc list element", tv, "cons", "list")
		}
		if keyFunc != nil {
			k = keyFunc.Call(s, slip.List{k}, d2)
		}
		if predicate.Call(s, slip.List{k}, d2) != nil {
			found = alist[i]
			break
		}
	}
	return
}
