// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Caaadr{Function: slip.Function{Name: "caaadr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "caaadr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list",
					Text: "The list to get a value from.",
				},
			},
			Return: "object",
			Text:   `__caaadr__ returns (car (car (car (cdr arg)))).`,
			Examples: []string{
				"(caaadr nil) => nil",
				"(caaadr '(a ((b c) d) e)) => b",
				"(setq x '(a ((b c) d) e))",
				"(setf (caaadr x) 'z) => z",
				" x => (a ((z c) d) e)",
			},
		}, &slip.CLPkg)
}

// Caaadr represents the caaadr function.
type Caaadr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Caaadr) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	a := args[0]
	switch list := a.(type) {
	case nil:
		return nil
	case slip.List:
		if 1 < len(list) {
			a = list[len(list)-2]
			for i := 2; 0 < i; i-- {
				switch list := a.(type) {
				case nil:
					return nil
				case slip.List:
					if 0 < len(list) {
						a = list[len(list)-1]
					}
				default:
					slip.PanicType("argument to caaadr", list, "list")
				}
			}
			return a
		}
	}
	return slip.PanicType("argument to caaadr", a, "list")
}

// Place a value in the first position of a list or cons.
func (f *Caaadr) Place(args slip.List, value slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	a := args[0]
	if list, ok := a.(slip.List); ok {
		if 1 < len(list) {
			a = list[len(list)-2]
			for i := 2; 0 < i; i-- {
				if list, _ := a.(slip.List); 0 < len(list) {
					if i == 1 {
						list[len(list)-1] = value
						return
					}
					a = list[len(list)-1]
				} else {
					break
				}
			}
		}
	}
	slip.PanicType("argument to caaadr", a, "list")
}
