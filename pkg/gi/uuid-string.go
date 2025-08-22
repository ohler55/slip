// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := UUIDString{Function: slip.Function{Name: "uuid-string", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "uuid-string",
			Args: []*slip.DocArg{
				{
					Name: "uuid",
					Type: "uuid",
					Text: "UUID to return the string representation of.",
				},
			},
			Return: "string",
			Text:   `__uuid-string__ returns a string representation of a UUID.`,
			Examples: []string{
				`(uuid-string (make-uuid)) => "6ef16994-701d-44d5-87ec-7ef3e2e5709b"`,
			},
		}, &Pkg)
}

// UUIDString represents the uuid-string function.
type UUIDString struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *UUIDString) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	u, ok := args[0].(UUID)
	if !ok {
		slip.TypePanic(s, depth, "uuid", args[0], "uuid")
	}
	return slip.String(u.IETF())
}
