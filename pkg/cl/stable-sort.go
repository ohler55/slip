// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"sort"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StableSort{Function: slip.Function{Name: "stable-sort", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "stable-sort",
			Args: []*slip.DocArg{
				{
					Name: "sequence",
					Type: "sequence",
					Text: `The _sequence_ to sort.`,
				},
				{Name: "&optional"},
				{
					Name: "predicate",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments and returns _t_ to
indicate the first argument is less that the second. The default is a built in comparitor.`,
				},
				{Name: "&key"},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _sequence_ to return a key for comparison.`,
				},
			},
			Return: "sequence",
			Text: `__stable-sort__ returns modified and stable sorted _sequence_. Deviating from
the Common LISP standard the _predicate_ is option. A default comparitor is used if
the _predicate_ is _nil_.`,
			Examples: []string{
				`(stable-sort "cba") => "abc"`,
				"(stable-sort '(b a c)) => (a b c)",
			},
		}, &slip.CLPkg)
}

// StableSort represents the stableSort function.
type StableSort struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *StableSort) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 4)
	result = args[0]
	var (
		keyFunc   slip.Caller
		predicate slip.Caller
	)
	if 1 < len(args) {
		if args[1] != nil {
			predicate = ResolveToCaller(s, args[1], depth)
		}
		for pos := 2; pos < len(args); pos += 2 {
			sym, ok := args[pos].(slip.Symbol)
			if !ok {
				slip.PanicType("keyword", args[pos], "keyword")
			}
			if strings.EqualFold(string(sym), ":key") && pos < len(args)-1 {
				keyFunc = ResolveToCaller(s, args[pos+1], depth)
			} else {
				slip.PanicType("keyword", sym, ":key")
			}
		}
	}
	switch ta := args[0].(type) {
	case nil:
		// leave as nil
	case slip.String:
		if 1 < len(ta) {
			ra := []rune(ta)
			list := make([]slip.Object, len(ra))
			for i, r := range ra {
				list[i] = slip.Character(r)
			}
			stableSortObjects(s, list, keyFunc, predicate, depth)
			for i, o := range list {
				ra[i] = rune(o.(slip.Character))
			}
			result = slip.String(ra)
		}
	case slip.List:
		if 1 < len(ta) {
			stableSortObjects(s, []slip.Object(ta), keyFunc, predicate, depth)
		}
	case *slip.Vector:
		elements := ta.AsList()
		if 1 < len(elements) {
			stableSortObjects(s, elements, keyFunc, predicate, depth)
		}
	default:
		slip.PanicType("sequence", ta, "sequence")
	}
	return
}

func stableSortObjects(s *slip.Scope, list []slip.Object, keyFunc slip.Caller, predicate slip.Caller, depth int) {
	d2 := depth + 1
	if predicate == nil {
		sort.SliceStable(list, func(i, j int) bool {
			vi := list[i]
			vj := list[j]
			if keyFunc != nil {
				vi = keyFunc.Call(s, slip.List{vi}, d2)
				vj = keyFunc.Call(s, slip.List{vj}, d2)
			}
			return sortLess(vi, vj)
		})
	} else {
		sort.SliceStable(list, func(i, j int) bool {
			vi := list[i]
			vj := list[j]
			if keyFunc != nil {
				vi = keyFunc.Call(s, slip.List{vi}, d2)
				vj = keyFunc.Call(s, slip.List{vj}, d2)
			}
			return predicate.Call(s, slip.List{vi, vj}, d2) != nil
		})
	}
}
