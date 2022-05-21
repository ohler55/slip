// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Send{Function: slip.Function{Name: "send", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "send",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "instance",
					Text: "The instance to invoke the method on.",
				},
				{
					Name: "message",
					Type: "symbol",
					Text: `The method name.`,
				},
				{Name: slip.AmpRest},
				{
					Name: "args",
					Type: "object",
					Text: `The remaining arguments to the method.`,
				},
			},
			Return: "object",
			Text:   `a message to an object.`,
			Examples: []string{
				"(setq fruit (make-instance 'strawberry :color 'red)) => #<strawberry 123456>",
				"(send fruit :color) => red",
			},
		}, &FlavorsPkg)
}

// Send represents the send function.
type Send struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Send) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {

	// TBD

	return nil
}
