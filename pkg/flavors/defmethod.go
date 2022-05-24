// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Defmethod{Function: slip.Function{Name: "defmethod", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "defmethod",
			Args: []*slip.DocArg{
				{
					Name: "method",
					Type: "list",
					// TBD is the order wrong? how about handling both?
					Text: "A list of the flavor name, optional method type, and operation (method name) .",
				},
				{
					Name: "args",
					Type: "lambda-list",
					Text: `The arguments to the method. A standard lambda-list`,
				},
				{Name: slip.AmpRest},
				{
					Name: "forms",
					Type: "object",
					Text: `The forms that process the method.`,
				},
			},
			Return: "nil",
			Text:   `defines a method for a flavor.`,
			Examples: []string{
				"(setq fruit (make-instance 'strawberry :color 'red)) => #<strawberry 123456>",
				"(defmethod (strawberry :size) => nil",
			},
		}, &FlavorsPkg)
}

// Defmethod represents the defmethod function.
type Defmethod struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Defmethod) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {

	// TBD same as defun to get a LispCaller
	// TBD should there be a lambda-list defun helper? shared by defun and defmethod and lambda
	//   maybe lambda plus forms to make LispCaller
	//  lispcaller.go - MakeLispCaller, new, build, def
	//    test with current defun

	return nil
}
