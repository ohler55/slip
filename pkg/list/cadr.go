// Copyright (c) 2022, Peter Ohler, All rights reserved.

package list

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cadr{Function: slip.Function{Name: "cadr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cadr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The value to take the second element of.",
				},
			},
			Return: "object",
			Text: `returns the _cadr_ if _arg_ is a _cons_, the second element if _arg_ is a _list_, and
_nil_ if _arg_ is _nil_ or an empty _list_.`,
			Examples: []string{
				"(cadr nil) => nil",
				"(cadr '(a b c)) => b",
				"(setq x '(a b c))",
				"(setf (cadr x) 'd) => d",
				" x => (a d c)",
			},
		})
}

// Cadr represents the cadr function.
type Cadr struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Cadr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	switch list := args[0].(type) {
	case nil:
		// leave result as nil
	case slip.List:
		if 1 < len(list) {
			result = list[len(list)-2]
		}
	default:
		slip.PanicType("argument to cadr", list, "list")
	}
	return
}

// Place a value in the first position of a list or cons.
func (f *Cadr) Place(args slip.List, value slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if list, ok := args[0].(slip.List); ok && 1 < len(list) {
		list[len(list)-2] = value
	} else {
		slip.PanicType("argument to cadr", list, "list")
	}
}
