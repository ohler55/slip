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
func (f *Send) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, -1)
	if self, ok := args[0].(slip.Receiver); ok {
		if method, ok := args[1].(slip.Symbol); ok {
			result = self.Receive(s, string(method), args[2:], depth)
		} else {
			slip.TypePanic(s, depth, "method of send", args[1], "keyword")
		}
	} else {
		slip.TypePanic(s, depth, "object of send", args[0], "instance")
	}
	return
}

// Place a value in the instance variable identified in the send.
func (f *Send) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.ArgCountCheck(f, args, 2, -1)
	self, ok := args[0].(*Instance)
	if !ok {
		slip.TypePanic(s, 0, "object of send", args[0], "instance")
	}
	if method, ok2 := args[1].(slip.Symbol); ok2 {
		_ = self.Receive(
			s,
			string(append([]byte(":set-"), string(method)[1:]...)),
			append(args[2:], value),
			0,
		)
		return
	}
	slip.TypePanic(s, 0, "method", args[1], "symbol")
}
