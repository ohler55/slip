// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Caadr{Function: slip.Function{Name: "caadr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "caadr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list",
					Text: "The list to get a value from.",
				},
			},
			Return: "object",
			Text:   `__caadr__ returns (car (car (cdr arg))).`,
			Examples: []string{
				"(caadr nil) => nil",
				"(caadr '(a (b c) d)) => b",
				"(setq x '(a (b c) d))",
				"(setf (caadr x) 'z) => z",
				" x => (a (z c) d)",
			},
		}, &slip.CLPkg)
}

// Caadr represents the caadr function.
type Caadr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Caadr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	switch list := args[0].(type) {
	case nil:
		break
	case slip.List:
		if 1 < len(list) {
			switch cadr := list[len(list)-2].(type) {
			case nil:
				return nil
			case slip.List:
				if 0 < len(cadr) {
					result = cadr[len(cadr)-1]
				}
			default:
				slip.PanicType("argument to caadr", cadr, "list")
			}
			return
		}
	default:
		slip.PanicType("argument to caadr", args[0], "list")
	}
	return nil
}

// Place a value in the first position of a list or cons.
func (f *Caadr) Place(args slip.List, value slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if list, ok := args[0].(slip.List); ok && 1 < len(list) {
		if list, _ = list[len(list)-2].(slip.List); 0 < len(list) {
			list[len(list)-1] = value
			return
		}
	}
	slip.PanicType("argument to caadr", args[0], "list")
}
