// Copyright (c) 2026, Peter Ohler, All rights reserved.

package wos

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

type removeCaller struct{}

func (caller removeCaller) Call(s *slip.Scope, args slip.List, depth int) (value slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":remove", len(args), 1, 1)
	key, ok := args[0].(slip.Fixnum)
	if !ok || key < 0 {
		slip.TypePanic(s, depth, "key", args[0], "non-negative fixnum")
	}
	if e, i := self.Any.(ordered).getEntry(uint64(key)); 0 <= i {
		oa := self.Any.(ordered)
		if len(oa)-1 == i {
			self.Any = oa[:len(oa)-1]
		} else {
			self.Any = append(oa[:i], oa[i+1:]...)
		}
		value = e.value
	}
	return
}

func (caller removeCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":remove",
		Text: `Removes the value at _key_ and returns the removed value.`,
		Args: []*slip.DocArg{
			{
				Name: "key",
				Type: "non-negative fixnum",
				Text: `A key to a value in the wos ordered set to remove.`,
			},
		},
		Return: "object",
	}
}

func defRemove() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := remove{Function: slip.Function{Name: "wos-remove", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "wos-remove",
			Args: []*slip.DocArg{
				{
					Name: "key",
					Type: "non-negative fixnum",
					Text: `A key to a value in the wos ordered set to remove.`,
				},
			},
			Return: "object",
			Text: `__wos-remove__ removes the value at _key_ and returns the removed value.

This is the same as the _:remove_ method of the _wos-flavor_ except none of the method's
daemons are invoked hence it remove a slight performance advantage.`,
			Examples: []string{
				`(setq wos (make-instance 'wos-flavor))`,
				`(wos-set wos 3 "three")`,
				`(wos-remove wos 3)`,
				`(wos-get wos 3) => nil`,
			},
		}, &Pkg)
}

type remove struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *remove) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	self, ok := args[0].(*flavors.Instance)
	if !ok || self.Type != flavor {
		slip.TypePanic(s, depth, "wos", args[0], "wos")
	}
	key, ok := args[1].(slip.Fixnum)
	if !ok || key < 0 {
		slip.TypePanic(s, depth, "key", args[1], "non-negative fixnum")
	}
	if e, i := self.Any.(ordered).getEntry(uint64(key)); 0 <= i {
		oa := self.Any.(ordered)
		if len(oa)-1 == i {
			self.Any = oa[:len(oa)-1]
		} else {
			self.Any = append(oa[:i], oa[i+1:]...)
		}
		result = e.value
	}
	return
}
