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
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	if eq(args[0], args[1]) {
		return slip.True
	}
	return nil
}

func eq(x, y slip.Object) bool {
	// Verify the types are the same.
	if (*[2]uintptr)(unsafe.Pointer(&x))[0] != (*[2]uintptr)(unsafe.Pointer(&y))[0] {
		return false
	}
	switch tx := x.(type) {
	case slip.Symbol:
		// Symbols with the same string are the same
		if y.(slip.Symbol) == tx {
			return true
		}
	default:
		if (*[2]uintptr)(unsafe.Pointer(&x))[1] == (*[2]uintptr)(unsafe.Pointer(&y))[1] {
			return true
		}
	}
	return false
}
