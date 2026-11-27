// Copyright (c) 2026, Peter Ohler, All rights reserved.

package wos

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defMake() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := make{Function: slip.Function{Name: "make-wos", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "make-wos",
			Return: "wos",
			Text:   `__make-wos__ makes a new _wos_.`,
			Examples: []string{
				`(make-wos) => #<wos-flavor 12345>`,
			},
		}, &Pkg)
}

// make represents the make-wos function.
type make struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *make) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 0)
	self := flavor.MakeInstance().(*flavors.Instance)
	self.Init(s, slip.List{}, depth)

	return self
}
