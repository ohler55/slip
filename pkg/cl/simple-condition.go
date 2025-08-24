// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

// SimpleConditionSymbol is the symbol with a value of "simple-condition".
const SimpleConditionSymbol = slip.Symbol("simple-condition")

// SimpleConditionNew returns a SimpleCondition object that can then be used
// with a call to panic.
func SimpleConditionNew(s *slip.Scope, depth int, ctrl string, args slip.List) slip.Object {
	c := slip.FindClass("simple-condition")
	obj := c.MakeInstance()
	obj.Init(s, slip.List{
		slip.Symbol(":format-control"), slip.String(ctrl),
		slip.Symbol(":format-arguments"), args,
	}, depth)

	return obj
}

// SimpleConditionPanic raises a SimpleConditionObj instance.
func SimpleConditionPanic(s *slip.Scope, depth int, ctrl string, args slip.List) {
	panic(SimpleConditionNew(s, depth, ctrl, args))
}

// SimpleCondMsg uses a format-control and format-arguments to generate a
// message for a simple-condition.
func SimpleCondMsg(s *slip.Scope, cond slip.Instance) (msg string) {
	if m, has := cond.SlotValue(slip.Symbol("message")); has && m != nil && m != slip.Unbound {
		if ss, ok := m.(slip.String); ok {
			msg = string(ss)
		} else {
			msg = cond.String()
		}
		cond.SetSlotValue(slip.Symbol("format-control"), slip.String("~A"))
		cond.SetSlotValue(slip.Symbol("format-arguments"), slip.List{m})
	} else {
		var args slip.List

		if sv, has := cond.SlotValue(slip.Symbol("format-control")); has {
			if ss, ok := sv.(slip.String); ok {
				args = append(args, ss)
			}
		}
		if sv, has := cond.SlotValue(slip.Symbol("format-arguments")); has {
			if list, ok := sv.(slip.List); ok {
				args = append(args, list...)
			}
		}
		if 0 < len(args) {
			msg = string(FormatArgs(s, args, 0))
		} else {
			msg = cond.String()
		}
	}
	return
}
