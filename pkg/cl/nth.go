// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Nth{Function: slip.Function{Name: "nth", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "nth",
			Args: []*slip.DocArg{
				{
					Name: "n",
					Type: "integer",
					Text: "The index into the list.",
				},
				{
					Name: "list",
					Type: "list",
					Text: "The value to return the _nth_ element of.",
				},
			},
			Return: "object",
			Text:   `__nth__ returns _n_ element of _list_ or _nil_ if outside the bounds of the _list_.`,
			Examples: []string{
				"(nth 0 nil) => nil",
				"(nth 1 '(a b c)) => b",
				"(setq x '(a b c))",
				"(setf (nth 1 x) 'd) => d",
				" x => (a d c)",
			},
		}, &slip.CLPkg)
}

// Nth represents the nth function.
type Nth struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Nth) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	switch list := args[1].(type) {
	case nil:
	case slip.List:
		num, ok := args[0].(slip.Integer)
		if !ok {
			slip.PanicType("n", args[0], "integer")
		}
		n := int(num.Int64())
		if len(list) <= n || n < 0 {
			return nil
		}
		return list[n]
	default:
		slip.PanicType("list", args[1], "list")
	}
	return nil
}

// Place a value in the first position of a list or cons.
func (f *Nth) Place(args slip.List, value slip.Object) {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	list, ok := args[1].(slip.List)
	if !ok {
		slip.PanicType("list", args[1], "list")
	}
	var num slip.Integer
	if num, ok = args[0].(slip.Integer); !ok {
		slip.PanicType("n", args[1], "integer")
	}
	n := int(num.Int64())
	if len(list) <= n || n < 0 {
		panic(fmt.Sprintf("%d is outside the bounds a list of length %d", n, len(list)))
	}
	list[n] = value
}
