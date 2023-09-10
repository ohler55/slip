// Copyright (c) 2022, Peter Ohler, All rights reserved.

package clos

import (
	"fmt"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
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

// MakeInstance represents the makeInstance function.
type MakeInstance struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *MakeInstance) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	c := classFromArg0(f, s, args, "make-instance")
	if c.NoMake() {
		slip.NewPanic("Can not create an instance of class or flavor %s.", c.Name())
	}
	inst := c.MakeInstance()
	inst.Init(s, args[1:], depth)

	return inst
}

func classFromArg0(f slip.Object, s *slip.Scope, args slip.List, label string) (class slip.Class) {
	slip.ArgCountCheck(f, args, 1, -1)
	switch ta := args[0].(type) {
	case slip.Symbol:
		if cf := flavors.Find(string(ta)); cf == nil {
			if c := Find(string(ta)); c == nil {
				slip.PanicClassNotFound(ta, "%s is not a defined class or flavor.", ta)
			} else {
				class = c
			}
		} else {
			class = cf
		}
	case *flavors.Flavor:
		class = ta
	case *Class:
		class = ta
	default:
		slip.PanicType(fmt.Sprintf("class argument to %s", label), ta, "symbol", "flavor", "class")
	}
	return
}
