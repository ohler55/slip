// Copyright (c) 2026, Peter Ohler, All rights reserved.

package wos

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

type setCaller struct{}

func (caller setCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":set", len(args), 2, 2)
	key, ok := args[0].(slip.Fixnum)
	if !ok || key < 0 {
		slip.TypePanic(s, depth, "key", args[0], "non-negative fixnum")
	}
	if e, _ := self.Any.(ordered).getEntry(uint64(key)); e != nil {
		e.value = args[1]
	} else {
		self.Any = append(self.Any.(ordered), entry{key: uint64(key), value: args[1]})
		self.Any.(ordered).sort()
	}
	return nil
}

func (caller setCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":set",
		Text: `Sets the value for _key_.`,
		Args: []*slip.DocArg{
			{
				Name: "key",
				Type: "non-negative fixnum",
				Text: `A key to a value in the wos ordered set.`,
			},
			{
				Name: "value",
				Type: "object",
				Text: `The value to set at the _key_ entry.`,
			},
		},
		Return: "nil",
	}
}

func defSet() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := set{Function: slip.Function{Name: "wos-set", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "wos-set",
			Args: []*slip.DocArg{
				{
					Name: "key",
					Type: "non-negative fixnum",
					Text: `A key to a value in the wos ordered set.`,
				},
				{
					Name: "value",
					Type: "object",
					Text: `The value to set at the _key_ entry.`,
				},
			},
			Return: "nil",
			Text: `__wos-set__ sets the _value_ for _key_.

This is the same as the _:set_ method of the _wos-flavor_ except none of the method's
daemons are invoked hence it set a slight performance advantage.`,
			Examples: []string{
				`(setq wos (make-instance 'wos-flavor))`,
				`(wos-set wos 3 "three")`,
				`(wos-get wos 3) => "three"`,
			},
		}, &Pkg)
}

type set struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *set) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 3, 3)
	self, ok := args[0].(*flavors.Instance)
	if !ok || self.Type != flavor {
		slip.TypePanic(s, depth, "wos", args[0], "wos")
	}
	key, ok := args[1].(slip.Fixnum)
	if !ok || key < 0 {
		slip.TypePanic(s, depth, "key", args[1], "non-negative fixnum")
	}
	if e, _ := self.Any.(ordered).getEntry(uint64(key)); e != nil {
		e.value = args[2]
	} else {
		self.Any = append(self.Any.(ordered), entry{key: uint64(key), value: args[2]})
		self.Any.(ordered).sort()
	}
	return nil
}
