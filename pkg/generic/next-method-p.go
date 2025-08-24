// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"github.com/ohler55/slip"
)

func defNextMethodP() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := NextMethodP{Function: slip.Function{Name: "next-method-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "next-method-p",
			Args:   []*slip.DocArg{},
			Return: "boolean",
			Text:   `__next-method-p__ returns true if __call-next-method__ will succeed.`,
		}, &Pkg)
}

// NextMethodP represents the next-method-p function.
type NextMethodP struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *NextMethodP) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	var loc *slip.WhopLoc
	if s.Has("~whopper-location~") {
		loc, _ = s.Get("~whopper-location~").(*slip.WhopLoc)
	}
	if loc == nil {
		slip.NewPanic("%s called outside an around method qualifier.", f.Name)
	}
	if loc.HasNext() {
		return slip.True
	}
	return nil
}
