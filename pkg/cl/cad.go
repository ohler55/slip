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
		case slip.Cons:
			if op {
				a = list.Car()
			} else {
				a = list.Cdr()
			}
		case slip.List:
			if 0 < len(list) {
				if op {
					a = list[len(list)-1]
				} else {
					a = list[:len(list)-1]
				}
			} else {
				a = nil
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
		switch list := a.(type) {
		case slip.Cons:
			if op {
				a = list.Car()
			} else {
				a = list.Cdr()
			}
		case slip.List:
			if 0 < len(list) {
				if op {
					a = list[len(list)-1]
				} else {
					a = list[:len(list)-1]
				}
			} else {
				a = nil
			}
		default:
			slip.PanicType(fmt.Sprintf("argument to %s", (f.(slip.Funky)).GetName()), args[0], "cons", "list")
		}
	}
	switch list := a.(type) {
	case slip.Cons:
		if ops[len(ops)-1] {
			if 0 < len(list) {
				list[len(list)-1] = value
				return
			}
		} else if 1 < len(list) {
			list[0] = value
			return
		}
	case slip.List:
		if ops[len(ops)-1] {
			if 0 < len(list) {
				list[len(list)-1] = value
				return
			}
		} else {
			panic("setf on cdr of a list is not support")
		}
	}
	slip.PanicType(fmt.Sprintf("argument to %s", (f.(slip.Funky)).GetName()), args[0], "cons", "list")
}
