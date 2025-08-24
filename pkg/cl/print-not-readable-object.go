// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

const objectSymbol = slip.Symbol("object")

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := PrintNotReadableObject{Function: slip.Function{Name: "print-not-readable-object", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "print-not-readable-object",
			Args: []*slip.DocArg{
				{
					Name: "condition",
					Type: "print-not-readable",
					Text: "The print-not-readable or subclass of print-not-readable to get the object of.",
				},
			},
			Return: "object",
			Text: `__print-not-readable-object__ returns the value of the _object_ slot in the _condition_
which must be a _print-not-readable_ or inherit from _print-not-readable_.`,
			Examples: []string{
				`(print-not-readable-object (make-condition 'print-not-readable :object 'test)) => test)`,
			},
		}, &slip.CLPkg)
}

// PrintNotReadableObject represents the print-not-readable-object function.
type PrintNotReadableObject struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *PrintNotReadableObject) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if ci, ok := args[0].(slip.Instance); !ok || !ci.IsA("print-not-readable") {
		slip.TypePanic(s, depth, "print-not-readable", args[0], "print-not-readable")
	} else {
		result, _ = ci.SlotValue(objectSymbol)
	}
	return
}
