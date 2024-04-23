// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"sort"
	"strings"
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Sort{Function: slip.Function{Name: "sort", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "sort",
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
			Text: `__sort__ returns modified and sorted _sequence_. Deviating from
the Common LISP standard the _predicate_ is option. A default comparitor is used if
the _predicate_ is _nil_.`,
			Examples: []string{
				`(sort "cba") => "abc"`,
				"(sort '(b a c)) => (a b c)",
			},
		}, &slip.CLPkg)
}

// Sort represents the sort function.
type Sort struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Sort) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
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
			sortObjects(s, list, keyFunc, predicate, depth)
			for i, o := range list {
				ra[i] = rune(o.(slip.Character))
			}
			result = slip.String(ra)
		}
	case slip.List:
		if 1 < len(ta) {
			sortObjects(s, []slip.Object(ta), keyFunc, predicate, depth)
		}
	case *slip.Vector:
		elements := ta.AsList()
		if 1 < len(elements) {
			sortObjects(s, elements, keyFunc, predicate, depth)
		}
	default:
		slip.PanicType("sequence", ta, "sequence")
	}
	return
}

func sortObjects(s *slip.Scope, list []slip.Object, keyFunc slip.Caller, predicate slip.Caller, depth int) {
	d2 := depth + 1
	if predicate == nil {
		sort.Slice(list, func(i, j int) bool {
			vi := list[i]
			vj := list[j]
			if keyFunc != nil {
				vi = keyFunc.Call(s, slip.List{vi}, d2)
				vj = keyFunc.Call(s, slip.List{vj}, d2)
			}
			return sortLess(vi, vj)
		})
	} else {
		sort.Slice(list, func(i, j int) bool {
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

func sortLess(v1, v2 slip.Object) bool {
	if v1 == slip.True {
		return true
	}
	if v2 == slip.True {
		return false
	}
	switch t1 := v1.(type) {
	case nil:
		return slip.True != v2
	case slip.Real:
		r1 := t1.RealValue()
		switch t2 := v2.(type) {
		case slip.Real:
			return r1 < t2.RealValue()
		case nil:
			return false
		default:
			return true
		}
	case slip.Character:
		switch t2 := v2.(type) {
		case slip.Real, nil:
			return false
		case slip.Character:
			return t1 < t2
		case slip.String:
			return 0 > strings.Compare(string([]rune{rune(t1)}), string(t2))
		case slip.Symbol:
			return 0 > strings.Compare(string([]rune{rune(t1)}), string(t2))
		default:
			return true
		}
	case slip.String:
		switch t2 := v2.(type) {
		case slip.Real, nil:
			return false
		case slip.String:
			return 0 > strings.Compare(string(t1), string(t2))
		case slip.Symbol:
			return 0 > strings.Compare(string(t1), string(t2))
		case slip.Character:
			return 0 > strings.Compare(string(t1), string([]rune{rune(t2)}))
		default:
			return true
		}
	case slip.Symbol:
		switch t2 := v2.(type) {
		case slip.Real, nil:
			return false
		case slip.String:
			return 0 > strings.Compare(string(t1), string(t2))
		case slip.Symbol:
			return 0 > strings.Compare(string(t1), string(t2))
		case slip.Character:
			return 0 > strings.Compare(string(t1), string([]rune{rune(t2)}))
		default:
			return true
		}
	case slip.Time:
		switch t2 := v2.(type) {
		case slip.String, slip.Symbol, slip.Real, slip.Character, nil:
			return false
		case slip.Time:
			return time.Time(t1).Before(time.Time(t2))
		default:
			return true
		}
	case slip.List:
		switch t2 := v2.(type) {
		case slip.List:
			for i, e1 := range t1 {
				if len(t2) <= i {
					return false
				}
				if !sortLess(e1, t2[i]) {
					if !slip.ObjectEqual(e1, t2[i]) {
						return false
					}
				}
			}
			return len(t1) < len(t2)
		case slip.Real, slip.String, slip.Symbol, slip.Character, slip.Time, nil:
			return false
		default:
			return true
		}
	}
	return false
}
