// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

	"github.com/ohler55/slip"
)

// Used by the various car-cdr combinations like cadar.

func cadGet(f slip.Object, args slip.List, ops []bool) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	a := args[0]
	for _, op := range ops {
		switch list := a.(type) {
		case nil:
			return nil
		case slip.List:
			if op {
				a = list.Car()
			} else {
				a = list.Cdr()
			}
		default:
			slip.PanicType(fmt.Sprintf("argument to %s", (f.(slip.Funky)).GetName()), args[0], "cons", "list")
		}
	}
	return a
}

func cadPlace(f slip.Object, args slip.List, ops []bool, value slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	a := args[0]
	for _, op := range ops[:len(ops)-1] {
		if list, ok := a.(slip.List); ok {
			if op {
				a = list.Car()
			} else {
				a = list.Cdr()
			}
		} else {
			slip.PanicType(fmt.Sprintf("argument to %s", (f.(slip.Funky)).GetName()), a, "cons", "list")
		}
	}
	if list, ok := a.(slip.List); ok {
		if ops[len(ops)-1] {
			if 0 < len(list) {
				list[0] = value
				return
			}
		} else {
			if len(list) == 2 {
				if _, ok = list[1].(slip.Tail); ok {
					list[1] = slip.Tail{Value: value}
					return
				}
			}
			slip.NewPanic("setf on cdr of a list is not supported")
		}
	}
	slip.PanicType(fmt.Sprintf("argument to %s", (f.(slip.Funky)).GetName()), a, "cons", "list")
}
