// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Caar{Function: slip.Function{Name: "caar", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "caar",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list",
					Text: "The value to take the first value of the first element of.",
				},
			},
			Return: "object",
			Text:   `__caar__ returns (car (car arg)).`,
			Examples: []string{
				"(caar nil) => nil",
				"(caar '((a b c d))) => a",
				"(setq x '((a b c d)))",
				"(setf (caar x) 'z) => z",
				" x => ((z b c d))",
			},
		}, &slip.CLPkg)
}

// Caar represents the caar function.
type Caar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Caar) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return caaGet(f, 2, args)
}

// Place a value in the first position of a list or cons.
func (f *Caar) Place(args slip.List, value slip.Object) {
	caaPlace(f, 2, args, value)
}

func caaGet(f slip.Object, n int, args slip.List) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	a := args[0]
	for i := n; 0 < i; i-- {
		switch list := a.(type) {
		case nil:
			a = nil
		case slip.Cons:
			a = list.Car()
		case slip.List:
			if 0 < len(list) {
				a = list[len(list)-1]
			}
		default:
			slip.PanicType(fmt.Sprintf("argument to %s", (f.(slip.Funky)).GetName()), args[0], "cons", "list")
		}
	}
	return a
}

// Used by caar, caaar, and caaaar.
func caaPlace(f slip.Object, n int, args slip.List, value slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	a := args[0]
	for i := n; 0 < i; i-- {
		switch list := a.(type) {
		case slip.Cons:
			if i == 1 {
				list[len(list)-1] = value
				return
			}
			a = list.Car()
		case slip.List:
			if i == 1 {
				list[len(list)-1] = value
				return
			}
			a = list[len(list)-1]
		default:
			slip.PanicType(fmt.Sprintf("argument to %s", (f.(slip.Funky)).GetName()), args[0], "cons", "list")
		}
	}
}
