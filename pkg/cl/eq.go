// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"unsafe"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Eq{Function: slip.Function{Name: "eq", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "eq",
			Args: []*slip.DocArg{
				{Name: "x", Type: "object"},
				{Name: "y", Type: "object"},
			},
			Return: "boolean",
			Text:   `__eq__ returns _t_ if _x_ and _y_ are the same object and _nil_ otherwise.`,
			Examples: []string{
				"(eq 5 5) => t",
				"(eq 5 5.0) => nil",
			},
		}, &slip.CLPkg)
}

// Eq represents the eq function.
type Eq struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Eq) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	// Verify the types are the same.
	if (*[2]uintptr)(unsafe.Pointer(&args[0]))[0] != (*[2]uintptr)(unsafe.Pointer(&args[1]))[0] {
		return nil
	}
	switch x := args[1].(type) {
	case slip.Symbol:
		// Symbols with the same string are the same
		if args[0].(slip.Symbol) == x {
			return slip.True
		}
	default:
		if (*[2]uintptr)(unsafe.Pointer(&args[0]))[1] == (*[2]uintptr)(unsafe.Pointer(&args[1]))[1] {
			return slip.True
		}
	}
	return nil
}
