// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"github.com/ohler55/slip"
)

func defCallNextMethod() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CallNextMethod{Function: slip.Function{Name: "call-next-method", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "call-next-method",
			Args: []*slip.DocArg{
				{Name: slip.AmpRest},
				{
					Name: "args",
					Type: "object",
					Text: `The remaining arguments to the method.`,
				},
			},
			Return: "object",
			Text:   `__call-next-method__ continues with the rest of the daemon methods using the arguments provided.`,
			Examples: []string{
				"(defmethod quux :around ((x fixnum)) (call-next-method))",
			},
		}, &Pkg)
}

// CallNextMethod represents the call-next-method function.
type CallNextMethod struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *CallNextMethod) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	var loc *slip.WhopLoc
	if s.Has("~whopper-location~") {
		loc, _ = s.Get("~whopper-location~").(*slip.WhopLoc)
	}
	if loc == nil {
		slip.NewPanic("%s called outside an around method qualifier.", f.Name)
	}
	if !loc.HasNext() {
		nnm := slip.MustFindFunc("no-next-method")
		gf := slip.MustFindFunc(loc.Method.Name)

		return nnm.Apply(s, append(slip.List{gf, loc.Method}, args...), depth)
	}
	return loc.Continue(s, args, depth)
}
