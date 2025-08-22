// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := UUIDValues{Function: slip.Function{Name: "uuid-values", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "uuid-values",
			Args: []*slip.DocArg{
				{
					Name: "uuid",
					Type: "uuid",
					Text: "UUID to return the values of.",
				},
			},
			Return: "values",
			Text: `__uuid-values__ returns a list of the two _fixnum_s that represent the
most significant 64 bits and the least significant 64 bits. A negative value is possible if
the high bit is set in the number.`,
			Examples: []string{
				`(uuid-values (make-uuid)) => (7994286899816383701 -8652401198136725349)`,
			},
		}, &Pkg)
}

// UUIDValues represents the uuid-values function.
type UUIDValues struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *UUIDValues) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	u, ok := args[0].(UUID)
	if !ok {
		slip.TypePanic(s, depth, "uuid", args[0], "uuid")
	}
	return slip.List{slip.Fixnum(u[0]), slip.Fixnum(u[1])}
}
