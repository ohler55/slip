// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Deftest{Function: slip.Function{Name: "deftest", Args: args, SkipEval: []bool{false, false, true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "deftest",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "string",
					Text: "The name of the test.",
				},
				{
					Name: "parent",
					Type: "suite-flavor instance",
					Text: "The parent suite or nil if no parent.",
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "form",
					Text: "Forms to evaluate during the test.",
				},
			},
			Return: "test-flavor instance",
			Text:   `__deftest__ creates a new _instance_ of the _test-flavor_.`,
			Examples: []string{
				`(deftest "tess" nil (assert-nil (car '())) => #<test-flavor 12345>`,
			},
		}, &Pkg)
}

// Deftest represents the deftest function.
type Deftest struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Deftest) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, -1)
	inst := testFlavor.MakeInstance().(*flavors.Instance)
	if name, ok := args[0].(slip.String); ok {
		inst.Let(slip.Symbol("name"), name)
	} else {
		slip.PanicType("name", args[0], "string")
	}
	inst.Let(slip.Symbol("parent"), args[1])
	if args[1] != nil {
		if parent, ok := args[1].(*flavors.Instance); !ok || parent.Flavor != suiteFlavor {
			slip.PanicType("parent", args[1], "instance of test-flavor")
		}
	}
	inst.Let(slip.Symbol("forms"), args[2:])
	_ = inst.Receive(s, ":init", nil, depth)

	return inst
}
