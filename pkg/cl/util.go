// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

func resolveToCaller(s *slip.Scope, fn slip.Object, depth int) slip.Caller {
	d2 := depth + 1
CallFunc:
	switch tf := fn.(type) {
	case *slip.Lambda:
		return tf
	case *slip.FuncInfo:
		return tf.Create(nil).(slip.Funky).Caller()
	case slip.Symbol:
		fn = slip.FindFunc(string(tf))
		goto CallFunc
	case slip.List:
		fn = s.Eval(tf, d2)
		goto CallFunc
	default:
		slip.PanicType("function", tf, "function")
	}
	return nil
}
