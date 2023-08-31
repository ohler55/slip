// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

// ResolveToCaller resolve an object to a slip.Caller.
func ResolveToCaller(s *slip.Scope, fn slip.Object, depth int) (caller slip.Caller) {
	d2 := depth + 1
CallFunc:
	switch tf := fn.(type) {
	case *slip.Lambda:
		caller = tf
	case *slip.FuncInfo:
		caller = tf.Create(nil).(slip.Funky).Caller()
	case slip.Symbol:
		fn = slip.FindFunc(string(tf))
		goto CallFunc
	case slip.List:
		fn = s.Eval(tf, d2)
		goto CallFunc
	default:
		slip.PanicType("function", tf, "function")
	}
	return
}

func objInList(obj slip.Object, list slip.List) bool {
	for _, x := range list {
		if slip.ObjectEqual(x, obj) {
			return true
		}
	}
	return false
}

func objInListTest(s *slip.Scope, obj slip.Object, list slip.List, testFunc slip.Caller, depth int) bool {
	for _, x := range list {
		if testFunc.Call(s, slip.List{x, obj}, depth) != nil {
			return true
		}
	}
	return false
}

func list2TestKeyArgs(
	s *slip.Scope,
	f slip.Object,
	args slip.List,
	depth int) (lists []slip.List, keyFunc slip.Caller, testFunc slip.Caller) {

	slip.ArgCountCheck(f, args, 2, 6)
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
				slip.NewPanic("%s missing an argument", sym)
			}
			switch strings.ToLower(string(sym)) {
			case ":key":
				keyFunc = ResolveToCaller(s, args[pos+1], depth)
			case ":test":
				testFunc = ResolveToCaller(s, args[pos+1], depth)
			default:
				slip.PanicType("keyword", sym, ":key", ":test")
			}
		}
	}
	return
}

func processBinding(s, ns *slip.Scope, arg slip.Object, depth int) {
	var bindings slip.List
	switch ta := arg.(type) {
	case nil:
	case slip.List:
		bindings = ta
	default:
		slip.PanicType("bindings", arg, "list")
	}
	for _, binding := range bindings {
		switch tb := binding.(type) {
		case slip.Symbol:
			ns.Let(tb, nil)
		case slip.List:
			if len(tb) < 1 {
				slip.PanicType("binding variable", nil, "list", "symbol")
			}
			sym, ok := tb[0].(slip.Symbol)
			if !ok {
				slip.PanicType("binding variable", tb[0], "symbol")
			}
			if 1 < len(tb) {
				// Use the original scope to avoid using the new bindings since
				// they are evaluated in apparent parallel.
				ns.Let(sym, slip.EvalArg(s, tb, 1, depth))
			} else {
				ns.Let(sym, nil)
			}
		default:
			slip.PanicType("binding", tb, "list", "symbol")
		}
	}
}

// callN is used by second, third, fourth, etc.
func callN(f slip.Object, args slip.List, n int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	a := args[0]
	switch list := a.(type) {
	case nil:
		// leave result as nil
	case slip.List:
		if n < len(list) {
			result = list[n]
			if tail, ok := result.(slip.Tail); ok {
				result = tail.Value
			}
		}
	default:
		slip.PanicType("list", list, "cons", "list")
	}
	return
}

// placeN is used by second, third, fourth, etc.
func placeN(f slip.Object, args slip.List, n int, value slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if list, ok := args[0].(slip.List); ok && n < len(list) {
		if _, ok := list[n].(slip.Tail); ok {
			value = slip.Tail{Value: value}
		}
		list[n] = value
		return
	}
	slip.PanicType("list", args[0], "cons", "list")
}
