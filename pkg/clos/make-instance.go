// Copyright (c) 2022, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

func defMakeInstance() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeInstance{Function: slip.Function{Name: "make-instance", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-instance",
			Args: []*slip.DocArg{
				{
					Name: "class",
					Type: "symbol",
					Text: "The name of the flavor or class to make an instance of.",
				},
				{Name: slip.AmpRest},
				{
					Name: "options",
					Type: "object",
					Text: `The remaining arguments must be pairs of an init-option and value.
An init-value can be the variable name prefixed with a colon or a plist option.`,
				},
			},
			Return: "instance",
			Text: `__make-instance__ makes an instance of _class_ with the provided
variable values and plist options.`,
			Examples: []string{
				"(make-instance 'strawberry :color 'red) => #<strawberry 123456>",
			},
		}, &Pkg)
}

// MakeInstance represents the make-instance function.
type MakeInstance struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *MakeInstance) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	class := classFromArg0(f, s, args, depth)
	if _, ok := class.(*ConditionClass); ok {
		slip.ErrorPanic(s, depth, "make-instance can not be used to make instances of %s. Use make-condition instead.",
			class.Name())
	}
	inst := class.MakeInstance()
	inst.Init(s, args[1:], depth)

	return inst
}

func classFromArg0(f slip.Object, s *slip.Scope, args slip.List, depth int) (class slip.Class) {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	switch ta := args[0].(type) {
	case slip.Symbol:
		if class = slip.FindClass(string(ta)); class == nil {
			slip.ErrorPanic(s, depth, "class %s not found.", ta)
		}
	case slip.Class:
		class = ta
	default:
		slip.TypePanic(s, depth, "class", ta, "symbol", "flavor", "class")
	}
	return
}
