// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := RassocIf{Function: slip.Function{Name: "rassoc-if", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "rassoc-if",
			Args: []*slip.DocArg{
				{
					Name: "predicate",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to the car of each element
in the _alist_. If non-nil is returned then the rassociated _cons_ is return.`,
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
			Return: "cons",
			Text: `__rassoc-if__ returns the first _cons_ whose _cdr_ satisfies _predicate_ or _nil_
if there is no match.`,
			Examples: []string{
				"(rassoc-if (lambda (k) (equal 'x k)) '((1 . x) (2 . y) (3 . z))) => (1 . x)",
			},
		}, &slip.CLPkg)
}

// RassocIf represents the rassoc-if function.
type RassocIf struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *RassocIf) Call(s *slip.Scope, args slip.List, depth int) (found slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 4)
	pos := 0
	predicate := ResolveToCaller(s, args[pos], depth)
	pos++
	alist, ok := args[pos].(slip.List)
	if !ok {
		slip.TypePanic(s, depth, "alist", args[pos], "list")
	}
	pos++
	var keyFunc slip.Caller
	for ; pos < len(args)-1; pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.TypePanic(s, depth, "keyword", args[pos], "keyword")
		}
		if keyword := strings.ToLower(string(sym)); keyword == ":key" {
			keyFunc = ResolveToCaller(s, args[pos+1], depth)
		} else {
			slip.TypePanic(s, depth, "keyword", sym, ":key")
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
			slip.TypePanic(s, depth, "rassoc list element", tv, "cons")
		}
		if keyFunc != nil {
			k = keyFunc.Call(s, slip.List{k}, d2)
		}
		if predicate.Call(s, slip.List{k}, d2) != nil {
			found = a
			break
		}
	}
	return
}
