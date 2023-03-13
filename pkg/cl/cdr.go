// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cdr{Function: slip.Function{Name: "cdr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cdr",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The value to take all but the first element of.",
				},
			},
			Return: "list|object",
			Text: `__cdr__ returns the _cdr_ if _arg_ is a _cons_, all but the first element if _arg_ is a _list_, and
_nil_ if _arg_ is _nil_ or an empty _list_.`,
			Examples: []string{
				"(cdr nil) => nil",
				"(cdr '(a . b) => b",
				"(cdr '(a b c)) => (b c)",
			},
		}, &slip.CLPkg)
}

// Cdr represents the cdr function.
type Cdr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cdr) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	a := args[0]
	switch list := a.(type) {
	case nil:
		// leave result as nil
	case slip.List:
		if 0 < len(list) {
			if tail, ok := list[0].(slip.Tail); ok {
				result = tail.Value
			} else {
				result = list[:len(list)-1]
			}
		}
	default:
		slip.PanicType("argument to cdr", list, "cons", "list")
	}
	return
}

// Place a value in the first position of a list or cons.
func (f *Cdr) Place(args slip.List, value slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	switch list := args[0].(type) {
	case slip.List:
		if 0 < len(list) {
			if _, ok := list[0].(slip.Tail); ok {
				list[0] = slip.Tail{Value: value}
				return
			}
		}
		panic("setf on cdr of a list is not implemented")
	default:
		slip.PanicType("argument to cdr", list, "cons", "list")
	}
}
