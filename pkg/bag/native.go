// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Native{Function: slip.Function{Name: "bag-native", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bag-native",
			Args: []*slip.DocArg{
				{
					Name: "bag",
					Type: "bag",
					Text: "The _bag_ to convert to a native LISP form.",
				},
			},
			Return: "bag",
			Text: `__bag-native__ returns the bag contents as a native LISP form.

This is the same as the _:native_ method of the _bag-flavor_ except none of the method's
daemons are invoked hence it native a slight performance advantage.`,
			Examples: []string{
				`(setq bag (make-instance 'bag-flavor :parse "{a:7}"))`,
				`(bag-native bag) => (("a" . 7))`,
			},
		}, &slip.CLPkg)
}

// Native represents the native function.
type Native struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Native) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	obj, ok := args[0].(*flavors.Instance)
	if !ok || obj.Flavor != flavor {
		slip.PanicType("bag", args[1], "bag")
	}
	return slip.SimpleObject(obj.Any)
}
