// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cadddr{Function: slip.Function{Name: "cadddr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cadddr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list",
					Text: "The value to take the fourth element of.",
				},
			},
			Return: "object",
			Text:   `__cadddr__ returns (car (cdr (cdr (cdr arg)))).`,
			Examples: []string{
				"(cadddr nil) => nil",
				"(cadddr '(a b c d)) => d",
				"(setq x '(a b c d))",
				"(setf (cadddr x) 'z) => z",
				" x => (a b d z)",
			},
		}, &slip.CLPkg)
}

// Cadddr represents the cadddr function.
type Cadddr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cadddr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	a := args[0]
	switch list := a.(type) {
	case nil:
		// leave result as nil
	case slip.List:
		if 3 < len(list) {
			result = list[len(list)-4]
		}
	default:
		slip.PanicType("argument to cadddr", list, "list")
	}
	return
}

// Place a value in the first position of a list or cons.
func (f *Cadddr) Place(args slip.List, value slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if list, ok := args[0].(slip.List); ok && 3 < len(list) {
		list[len(list)-4] = value
	} else {
		slip.PanicType("argument to cadddr", list, "list")
	}
}
