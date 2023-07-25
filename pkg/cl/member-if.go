// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MemberIf{Function: slip.Function{Name: "member-if", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "member-if",
			Args: []*slip.DocArg{
				{
					Name: "predicate",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments and returns a boolean to
indicate a match.`,
				},
				{
					Name: "list",
					Type: "list",
					Text: "The list to search for a match with _predicate_.",
				},
				{Name: "&key"},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _list_ to return a key for comparison.`,
				},
			},
			Return: "list",
			Text:   `__member-if__ returns the tail of the _list_ headed by the first match to _item_.`,
			Examples: []string{
				"(member-if 'null '(a nil b)) => (nil b)",
			},
		}, &slip.CLPkg)
}

// MemberIf represents the member-if function.
type MemberIf struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MemberIf) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 6)
	predicate := ResolveToCaller(s, args[0], depth)
	var list slip.List
	switch ta := args[1].(type) {
	case nil:
		return nil
	case slip.List:
		list = ta
	default:
		slip.PanicType("list", args[1], "list")
	}
	var keyFunc slip.Caller
	pos := 2
	for ; pos < len(args)-1; pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		keyword := strings.ToLower(string(sym))
		switch keyword {
		case ":key":
			keyFunc = ResolveToCaller(s, args[pos+1], depth)
		default:
			slip.PanicType("keyword", sym, ":key")
		}
	}
	if pos < len(args) {
		panic("extra arguments that are not keyword and value pairs")
	}
	d2 := depth + 1
	for i, a := range list {
		k := a
		if keyFunc != nil {
			k = keyFunc.Call(s, slip.List{k}, d2)
		}
		if predicate.Call(s, slip.List{k}, d2) != nil {
			result = list[i:]
			break
		}
	}
	return
}
