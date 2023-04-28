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
			Text:   `__send__ a _message_ to an _object_.`,
			Examples: []string{
				"(setq fruit (make-instance 'strawberry :color 'red)) => #<strawberry 123456>",
				"(send fruit :color) => red",
			},
		}, &Pkg)
}

// Send represents the send function.
type Send struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Send) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) < 2 {
		slip.PanicArgCount(f, 2, -1)
	}
	receiver, ok := args[0].(Receiver)
	if !ok {
		slip.PanicType("object of send", args[0], "instance")
	}
	var method slip.Symbol
	if method, ok = args[1].(slip.Symbol); !ok {
		slip.PanicType("method of send", args[1], "keyword")
	}
	return receiver.Receive(string(method), args[2:], depth)
}
