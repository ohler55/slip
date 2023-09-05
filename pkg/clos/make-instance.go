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
					Name: "flavor",
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
			Text: `__make-instance__ makes an instance of _flavor_ or _class_ with the provided
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
	inst := c.MakeInstance()
	inst.Init(s, args[1:], depth)

	return inst
}

func classFromArg0(f slip.Object, s *slip.Scope, args slip.List, label string) (class slip.Class) {
	slip.ArgCountCheck(f, args, 1, -1)
	switch ta := args[0].(type) {
	case slip.Symbol:
		class = flavors.Find(string(ta))
		// TBD look for built-in class if class is nil
		if class == nil {
			slip.PanicClassNotFound(ta, "%s is not a defined flavor.", ta)
		}
	case *flavors.Flavor:
		class = ta
	default:
		slip.PanicType(fmt.Sprintf("flavor argument to %s", label), ta, "symbol", "flavor", "class")
	}
	if class.Abstract() {
		slip.NewPanic("Can not create an instance of abstract flavor %s.", class.Name())
	}
	return
}
