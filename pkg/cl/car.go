// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Car{Function: slip.Function{Name: "car", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "car",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The value to take the first element of.",
				},
			},
			Return: "object",
			Text: `__car__ returns the _car_ if _arg_ is a _cons_, the first element if _arg_ is a _list_, and
_nil_ if _arg_ is _nil_ or an empty _list_.`,
			Examples: []string{
				"(car nil) => nil",
				"(car '(a . b) => a",
				"(car '(a b c)) => a",
			},
		}, &slip.CLPkg)
}

// Car represents the car function.
type Car struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Car) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	a := args[0]
	switch list := a.(type) {
	case nil:
		// leave result as nil
	case slip.List:
		result = list.Car()
	default:
		slip.PanicType("argument to car", list, "cons", "list")
	}
	return
}

// Place a value in the first position of a list or cons.
func (f *Car) Place(s *slip.Scope, args slip.List, value slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if list, ok := args[0].(slip.List); ok && 0 < len(list) {
		list[0] = value
		return
	}
	slip.PanicType("argument to car", args[0], "cons", "list")
}
