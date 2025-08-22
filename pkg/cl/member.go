// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Member{Function: slip.Function{Name: "member", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "member",
			Args: []*slip.DocArg{
				{
					Name: "item",
					Type: "object",
					Text: "The value to search for in _list_.",
				},
				{
					Name: "list",
					Type: "list",
					Text: "The list to search for _item_ in.",
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
			Text:   `__member__ returns the tail of the _list_ headed by the first match to _item_.`,
			Examples: []string{
				"(member 'b '(a b c)) => (b c)",
			},
		}, &slip.CLPkg)
}

// Member represents the member function.
type Member struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Member) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 6)
	item := args[0]
	var list slip.List
	switch ta := args[1].(type) {
	case nil:
		return nil
	case slip.List:
		list = ta
	default:
		slip.TypePanic(s, depth, "list", args[1], "list")
	}
	var (
		keyFunc  slip.Caller
		testFunc slip.Caller
	)
	pos := 2
	for ; pos < len(args)-1; pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.TypePanic(s, depth, "keyword", args[pos], "keyword")
		}
		keyword := strings.ToLower(string(sym))
		switch keyword {
		case ":key":
			keyFunc = ResolveToCaller(s, args[pos+1], depth)
		case ":test":
			testFunc = ResolveToCaller(s, args[pos+1], depth)
		default:
			slip.TypePanic(s, depth, "keyword", sym, ":key", ":test")
		}
	}
	if pos < len(args) {
		slip.NewPanic("extra arguments that are not keyword and value pairs")
	}
	d2 := depth + 1
	for i, a := range list {
		k := a
		if keyFunc != nil {
			k = keyFunc.Call(s, slip.List{k}, d2)
		}
		if testFunc == nil {
			if slip.ObjectEqual(item, k) {
				result = list[i:]
				break
			}
		} else if testFunc.Call(s, slip.List{item, k}, d2) != nil {
			result = list[i:]
			break
		}
	}
	return
}
