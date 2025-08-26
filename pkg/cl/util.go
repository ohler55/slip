// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"
	"plugin"
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
	case slip.Funky:
		caller = tf.Caller()
	case slip.Symbol:
		fn = slip.MustFindFunc(string(tf))
		goto CallFunc
	case slip.List:
		fn = s.Eval(tf, d2)
		goto CallFunc
	default:
		slip.TypePanic(s, depth, "function", tf, "function")
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

	slip.CheckArgCount(s, depth, f, args, 2, 6)
	switch ta := args[0].(type) {
	case nil:
		// ok
	case slip.List:
		lists = append(lists, ta)
	default:
		slip.TypePanic(s, depth, "list-1", ta, "list")
	}
	switch ta := args[1].(type) {
	case nil:
		// ok
	case slip.List:
		lists = append(lists, ta)
	default:
		slip.TypePanic(s, depth, "list-2", ta, "list")
	}
	if 2 < len(args) {
		for pos := 2; pos < len(args); pos += 2 {
			sym, ok := args[pos].(slip.Symbol)
			if !ok {
				slip.TypePanic(s, depth, "keyword", args[pos], "keyword")
			}
			if len(args)-1 <= pos {
				slip.ErrorPanic(s, depth, "%s missing an argument", sym)
			}
			switch strings.ToLower(string(sym)) {
			case ":key":
				keyFunc = ResolveToCaller(s, args[pos+1], depth)
			case ":test":
				testFunc = ResolveToCaller(s, args[pos+1], depth)
			default:
				slip.TypePanic(s, depth, "keyword", sym, ":key", ":test")
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
		slip.TypePanic(s, depth, "bindings", arg, "list")
	}
	for _, binding := range bindings {
		switch tb := binding.(type) {
		case slip.Symbol:
			ns.Let(tb, nil)
		case slip.List:
			if len(tb) < 1 {
				slip.TypePanic(s, depth, "binding variable", nil, "list", "symbol")
			}
			sym, ok := tb[0].(slip.Symbol)
			if !ok {
				slip.TypePanic(s, depth, "binding variable", tb[0], "symbol")
			}
			if 1 < len(tb) {
				// Use the original scope to avoid using the new bindings since
				// they are evaluated in apparent parallel.
				ns.Let(sym, slip.EvalArg(s, tb, 1, depth))
			} else {
				ns.Let(sym, nil)
			}
		default:
			slip.TypePanic(s, depth, "binding", tb, "list", "symbol")
		}
	}
}

// callN is used by second, third, fourth, etc.
func callN(s *slip.Scope, f slip.Object, args slip.List, n, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
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
		slip.TypePanic(s, depth, "list", list, "cons", "list")
	}
	return
}

// placeN is used by second, third, fourth, etc.
func placeN(s *slip.Scope, f slip.Object, args slip.List, n int, value slip.Object) {
	slip.CheckArgCount(s, 0, f, args, 1, 1)
	if list, ok := args[0].(slip.List); ok && n < len(list) {
		if _, ok := list[n].(slip.Tail); ok {
			value = slip.Tail{Value: value}
		}
		list[n] = value
		return
	}
	slip.TypePanic(s, 0, "list", args[0], "cons", "list")
}

func reverseBytes(buf []byte) {
	i, j := 0, len(buf)-1
	for i < j {
		buf[i], buf[j] = buf[j], buf[i]
		i++
		j--
	}
}

func fixnumRevBytes(fn slip.Fixnum) []byte {
	u := uint64(fn)
	return []byte{
		byte(u & 0xff),
		byte((u >> 8) & 0xff),
		byte((u >> 16) & 0xff),
		byte((u >> 24) & 0xff),
		byte((u >> 32) & 0xff),
		byte((u >> 40) & 0xff),
		byte((u >> 48) & 0xff),
		byte((u >> 56) & 0xff),
	}
}

func complement(v slip.Object) (result slip.Object) {
	switch tv := v.(type) {
	case slip.Fixnum:
		result = slip.Fixnum(^uint64(tv))
	case *slip.Bignum:
		var bi big.Int
		bi.Add((*big.Int)(tv), big.NewInt(1))
		bi.Neg(&bi)
		result = (*slip.Bignum)(&bi)
	}
	return
}

func bitOpArrays(s *slip.Scope, f slip.Object, args slip.List, depth int) (a1, a2, r slip.ArrayLike) {
	slip.CheckArgCount(s, depth, f, args, 2, 3)
	switch t1 := args[0].(type) {
	case *slip.Array:
		t2, ok := args[1].(*slip.Array)
		if !ok {
			slip.TypePanic(s, depth, "bit-array2", args[1], "bit-array")
		}
		if t1.ElementType() != slip.BitSymbol {
			slip.TypePanic(s, depth, "bit-array1", t1, "bit-array")
		}
		if t2.ElementType() != slip.BitSymbol {
			slip.TypePanic(s, depth, "bit-array2", t2, "bit-array")
		}
		d1 := t1.Dimensions()
		d2 := t2.Dimensions()
		if len(d1) != len(d2) {
			slip.ErrorPanic(s, depth, "%s and %s do not have the same dimensions.", t1, t2)
		}
		for i, d := range d1 {
			if d2[i] != d {
				slip.ErrorPanic(s, depth, "%s and %s do not have the same dimensions.", t1, t2)
			}
		}
		if 2 < len(args) {
			ra, ok := args[2].(*slip.Array)
			if !ok || ra.ElementType() != slip.BitSymbol {
				slip.TypePanic(s, depth, "opt-arg", args[2], "bit-array")
			}
			dr := ra.Dimensions()
			if len(d1) != len(dr) {
				slip.ErrorPanic(s, depth, "%s and %s do not have the same dimensions.", t1, ra)
			}
			for i, d := range d1 {
				if dr[i] != d {
					slip.ErrorPanic(s, depth, "%s and %s do not have the same dimensions.", t1, ra)
				}
			}
			r = ra
		} else {
			r = slip.NewArray(d1, slip.BitSymbol, slip.Bit(0), nil, false)
		}
		a1 = t1
		a2 = t2
	case *slip.BitVector:
		t2, ok := args[1].(*slip.BitVector)
		if !ok {
			slip.TypePanic(s, depth, "bit-array2", args[1], "bit-array")
		}
		if t1.Length() != t2.Length() {
			slip.ErrorPanic(s, depth, "%s and %s do not have the same dimensions.", t1, t2)
		}
		if 2 < len(args) {
			ra, ok := args[2].(*slip.BitVector)
			if !ok {
				slip.TypePanic(s, depth, "opt-arg", args[2], "bit-array")
			}
			if t1.Len != ra.Len {
				slip.ErrorPanic(s, depth, "%s and %s do not have the same dimensions.", t1, ra)
			}
			r = ra
		} else {
			r = &slip.BitVector{
				Bytes:   make([]byte, len(t1.Bytes)),
				Len:     t1.Len,
				FillPtr: -1,
			}
		}
		a1 = t1
		a2 = t2
	default:
		slip.TypePanic(s, depth, "bit-array1", t1, "bit-array")
	}
	return
}

// OpenPlugin attempts to open a plugin. If the attempt fails due to already
// loaded then the panic is ignored.
func OpenPlugin(filepath string) {
	if _, err := plugin.Open(filepath); err != nil {
		// The error is a error string so just check the contents.
		if !strings.Contains(err.Error(), "plugin already loaded") {
			slip.ErrorPanic(slip.NewScope(), 0, "plugin %s open failed. %s", filepath, err)
		}
	}
}
