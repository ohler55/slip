// Copyright (c) 2026, Peter Ohler, All rights reserved.

package wos

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

type getCaller struct{}

func (caller getCaller) Call(s *slip.Scope, args slip.List, depth int) (value slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":get", len(args), 1, 1)
	key, ok := args[0].(slip.Fixnum)
	if !ok || key < 0 {
		slip.TypePanic(s, depth, "key", args[0], "non-negative fixnum")
	}
	if e, _ := self.Any.(ordered).getEntry(uint64(key)); e != nil {
		value = e.value
	}
	return
}

func (caller getCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":get",
		Text: `Gets a _value_ indexed by _key_.`,
		Args: []*slip.DocArg{
			{
				Name: "key",
				Type: "non-negative fixnum",
				Text: `A key to a value in the wos ordered set.`,
			},
		},
		Return: "object",
	}
}

func defGet() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := get{Function: slip.Function{Name: "wos-get", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "wos-get",
			Args: []*slip.DocArg{
				{
					Name: "key",
					Type: "non-negative fixnum",
					Text: `A key to a value in the wos ordered set.`,
				},
			},
			Return: "object",
			Text: `__wos-get__ gets a _value_ indexed by _key_ from a wos-flavor instance.

This is the same as the _:get_ method of the _wos-flavor_ except none of the method's
daemons are invoked hence it has a slight performance advantage.`,
			Examples: []string{
				`(setq wos (make-instance 'wos-flavor))`,
				`(wos-set wos 3 "three")`,
				`(wos-get wos 3) => "three"`,
			},
		}, &Pkg)
}

type get struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *get) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
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
		result = e.value
	}
	return
}

// Place a value in the wos at the specified key.
func (f *get) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.CheckArgCount(s, 0, f, args, 2, 2)
	self, ok := args[0].(*flavors.Instance)
	if !ok || self.Type != flavor {
		slip.TypePanic(s, 0, "wos", args[0], "wos")
	}
	key, ok := args[1].(slip.Fixnum)
	if !ok || key < 0 {
		slip.TypePanic(s, 0, "key", args[1], "non-negative fixnum")
	}
	if e, _ := self.Any.(ordered).getEntry(uint64(key)); e != nil {
		e.value = value
	} else {
		self.Any = append(self.Any.(ordered), entry{key: uint64(key), value: value})
		self.Any.(ordered).sort()
	}
}
