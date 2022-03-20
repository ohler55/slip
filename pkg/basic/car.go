// Copyright (c) 2022, Peter Ohler, All rights reserved.

package basic

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object { return &Car{Function: slip.Function{Name: "car", Args: args}} },
		&slip.FuncDoc{
			Name: "car",
			Args: []*slip.DocArg{
				{
					Name:     "arg",
					Type:     "list|cons",
					Text:     "The value to take the first element of.",
					Optional: false,
				},
			},
			Return: "object",
			Text: `returns the _car_ if _arg_ is a _cons_, the first element if _arg_ is a _list_, and
_nil_ if _arg_ is _nil_ or an empty _list_.`,
			Examples: []string{
				"(car nil) => nil",
				"(car '(a . b) => a",
				"(car '(a b c)) => a",
			},
			HasKeys: false,
			HasRest: false,
		})
}

// Car represents the car function.
type Car struct {
	slip.Function
}

// Eval the object.
func (f *Car) Eval(s *slip.Scope, depth int) (result slip.Object) {
	if len(f.Args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	s.Before(f, depth)
	defer s.After(f, depth)
	if arg := f.Args[0]; arg != nil {
		switch list := arg.Eval(s, depth+1).(type) {
		case nil:
			// leave result as nil
		case slip.Cons:
			if 0 < len(list) {
				result = list[0]
			}
		case slip.List:
			if 0 < len(list) {
				result = list[0]
			}
		default:
			slip.PanicType("argument to car", arg, "cons", "list")
		}
	}
	return
}
