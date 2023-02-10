// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
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
				"(caar '(a (b c d))) => d",
				"(setq x '(a (b c d)))",
				"(setf (caar x) 'z) => z",
				" x => (a (z c d))",
			},
		}, &slip.CLPkg)
}

// Caar represents the caar function.
type Caar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Caar) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	a := args[0]
	for i := 2; 0 < i; i-- {
		switch list := a.(type) {
		case nil:
			a = nil
		case slip.List:
			if 0 < len(list) {
				a = list[len(list)-1]
			}
		default:
			slip.PanicType("argument to caar", list, "list")
		}
	}
	return a
}

// Place a value in the first position of a list or cons.
func (f *Caar) Place(args slip.List, value slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	a := args[0]
	for i := 2; 0 < i; i-- {
		if list, _ := a.(slip.List); 0 < len(list) {
			if i == 1 {
				list[len(list)-1] = value
				return
			}
			a = list[len(list)-1]
		} else {
			slip.PanicType("argument to caar", a, "list")
		}
	}
}
