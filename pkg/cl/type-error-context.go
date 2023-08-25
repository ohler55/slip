// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TypeErrorContext{Function: slip.Function{Name: "type-error-context", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "type-error-context",
			Args: []*slip.DocArg{
				{
					Name: "condition",
					Type: "type-error",
					Text: "The type-error or subclass of type-error to get the context of.",
				},
			},
			Return: "object",
			Text: `__type-error-context__ returns the value of the _context_ slot in the _condition_
which must be of type _type-error_ or inherit from _type-error_.`,
			Examples: []string{
				`(type-error-context (make-condition 'type-error :context 'test)) => test)`,
			},
		}, &slip.CLPkg)
}

// TypeErrorContext represents the type-error-context function.
type TypeErrorContext struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TypeErrorContext) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	cond, ok := args[0].(slip.TypeError)
	if !ok {
		slip.PanicUnboundSlot(args[0], slip.Symbol("context"), "")
	}
	return cond.Context()
}
