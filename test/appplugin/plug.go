// Copyright (c) 2025, Peter Ohler, All rights reserved.

package main

import (
	"github.com/ohler55/slip"
	_ "github.com/ohler55/slip/pkg"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Plug{Function: slip.Function{Name: "app-plug", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "app-plug",
			Args:   []*slip.DocArg{},
			Return: "string",
		}, &Pkg)
}

// Plug represents the plug function.
type Plug struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Plug) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return slip.String("Plugged it")
}
