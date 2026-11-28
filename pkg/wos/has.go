// Copyright (c) 2026, Peter Ohler, All rights reserved.

package wos

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

type hasCaller struct{}

func (caller hasCaller) Call(s *slip.Scope, args slip.List, depth int) (value slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":has", len(args), 1, 1)
	key, ok := args[0].(slip.Fixnum)
	if !ok || key < 0 {
		slip.TypePanic(s, depth, "key", args[0], "non-negative fixnum")
	}
	if e, _ := self.Any.(ordered).getEntry(uint64(key)); e != nil {
		value = slip.True
	}
	return
}

func (caller hasCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":has",
		Text: `Returns true if there is a value associated with _key_.`,
		Args: []*slip.DocArg{
			{
				Name: "key",
				Type: "non-negative fixnum",
				Text: `A key to a value in the wos ordered set.`,
			},
		},
		Return: "boolean",
	}
}

func defHas() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := has{Function: slip.Function{Name: "wos-has", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "wos-has",
			Args: []*slip.DocArg{
				{
					Name: "key",
					Type: "non-negative fixnum",
					Text: `A key to a value in the wos ordered set.`,
				},
			},
			Return: "object",
			Text: `__wos-has__ returns if there is a value associated with _key_.

This is the same as the _:has_ method of the _wos-flavor_ except none of the method's
daemons are invoked hence it has a slight performance advantage.`,
			Examples: []string{
				`(setq wos (make-instance 'wos-flavor))`,
				`(wos-set wos 3 "three")`,
				`(wos-has wos 3) => t`,
			},
		}, &Pkg)
}

type has struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *has) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	self, ok := args[0].(*flavors.Instance)
	if !ok || self.Type != flavor {
		slip.TypePanic(s, depth, "wos", args[0], "wos")
	}
	key, ok := args[1].(slip.Fixnum)
	if !ok || key < 0 {
		slip.TypePanic(s, depth, "key", args[1], "non-negative fixnum")
	}
	if e, _ := self.Any.(ordered).getEntry(uint64(key)); e != nil {
		result = slip.True
	}
	return
}
