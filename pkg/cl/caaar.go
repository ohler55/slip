// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Caaar{Function: slip.Function{Name: "caaar", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "caaar",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list",
					Text: "The list to get a value from.",
				},
			},
			Return: "object",
			Text:   `__caaar__ returns (car (car (car arg))).`,
			Examples: []string{
				"(caaar nil) => nil",
				"(caaar '(((a b) c) d)) => a",
				"(setq x '(((a b) c) d))",
				"(setf (caaar x) 'z) => z",
				" x => (((z b) c) d)",
			},
		}, &slip.CLPkg)
}

// Caaar represents the caaar function.
type Caaar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Caaar) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	a := args[0]
	for i := 3; 0 < i; i-- {
		switch list := a.(type) {
		case nil:
			a = nil
		case slip.List:
			if 0 < len(list) {
				a = list[len(list)-1]
			}
		default:
			slip.PanicType("argument to caaar", list, "list")
		}
	}
	return a
}

// Place a value in the first position of a list or cons.
func (f *Caaar) Place(args slip.List, value slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	a := args[0]
	for i := 3; 0 < i; i-- {
		if list, _ := a.(slip.List); 0 < len(list) {
			if i == 1 {
				list[len(list)-1] = value
				return
			}
			a = list[len(list)-1]
		} else {
			slip.PanicType("argument to caaar", a, "list")
		}
	}
}
