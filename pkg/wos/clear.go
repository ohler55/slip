// Copyright (c) 2026, Peter Ohler, All rights reserved.

package wos

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

type clearCaller struct{}

func (caller clearCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":clear", len(args), 0, 0)
	self.Any = ordered{}

	return nil
}

func (caller clearCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":clear",
		Text:   `Clears a wos by removing all values.`,
		Return: "nil",
	}
}

func defClear() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := clear{Function: slip.Function{Name: "wos-clear", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "wos-clear",
			Return: "nil",
			Text: `__wos-clear__ clears a wos by removing all values.

This is the same as the _:clear_ method of the _wos-flavor_ except none of the method's
daemons are invoked hence it has a slight performance advantage.`,
			Examples: []string{
				`(setq wos (make-instance 'wos-flavor))`,
				`(wos-set wos 3 "three")`,
				`(wos-clear wos)`,
				`(wos-get wos 3) => nil`,
			},
		}, &Pkg)
}

type clear struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *clear) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	self, ok := args[0].(*flavors.Instance)
	if !ok || self.Type != flavor {
		slip.TypePanic(s, depth, "wos", args[0], "wos")
	}
	self.Any = ordered{}

	return nil
}
