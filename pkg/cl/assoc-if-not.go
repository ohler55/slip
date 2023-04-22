// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := AssocIfNot{Function: slip.Function{Name: "assoc-if-not", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "assoc-if-not",
			Args: []*slip.DocArg{
				{
					Name: "predicate",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to the car of each element
in the _alist_. If nil is returned then the associated _cons_ is return.`,
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
			Text: `__assoc-if-not__ returns the first _cons_ whose _car_ does not satisfy _predicate_ or _nil_
if there is no match.`,
			Examples: []string{
				"(assoc-if-not (lambda (k) (equal 'x k)) '((x . 1) (y. 2) (z . 3))) => (y . 2)",
			},
		}, &slip.CLPkg)
}

// AssocIfNot represents the assocIf function.
type AssocIfNot struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *AssocIfNot) Call(s *slip.Scope, args slip.List, depth int) (found slip.Object) {
	slip.ArgCountCheck(f, args, 2, 4)
	pos := 0
	predicate := ResolveToCaller(s, args[pos], depth)
	pos++
	alist, ok := args[pos].(slip.List)
	if !ok {
		slip.PanicType("alist", args[pos], "list")
	}
	pos++
	var keyFunc slip.Caller
	for ; pos < len(args)-1; pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		if keyword := strings.ToLower(string(sym)); keyword == ":key" {
			keyFunc = ResolveToCaller(s, args[pos+1], depth)
		} else {
			slip.PanicType("keyword", sym, ":key")
		}
	}
	d2 := depth + 1
	var k slip.Object
	for _, a := range alist {
		switch tv := a.(type) {
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
		if predicate.Call(s, slip.List{k}, d2) == nil {
			found = a
			break
		}
	}
	return
}
