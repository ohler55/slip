// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Defsuite{Function: slip.Function{Name: "defsuite", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "defsuite",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "string",
					Text: "The name of the suite.",
				},
				{
					Name: "parent",
					Type: "suite-flavor instance",
					Text: "The parent suite or nil if no parent.",
				},
				{Name: "&key"},
				{
					Name: "setup",
					Type: "function",
					Text: "A function to call before running the tests in the suite.",
				},
				{
					Name: "teardown",
					Type: "function",
					Text: "A function to call after running the tests in the suite.",
				},
			},
			Return: "suite-flavor instance",
			Text:   `__defsuite__ creates a new _instance_ of the _suite-flavor_.`,
			Examples: []string{
				`(defsuite "sweet" nil) => #<suite-flavor 12345>`,
			},
		}, &Pkg)
}

// Defsuite represents the defsuite function.
type Defsuite struct {
	slip.Function
}

const (
	setupKey    = slip.Symbol(":setup")
	teardownKey = slip.Symbol(":teardown")
)

// Call the function with the arguments provided.
func (f *Defsuite) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 6)
	inst := suiteFlavor.MakeInstance().(*flavors.Instance)
	if name, ok := args[0].(slip.String); ok {
		inst.Let(slip.Symbol("name"), name)
	} else {
		slip.TypePanic(s, depth, "name", args[0], "string")
	}
	inst.Let(slip.Symbol("parent"), args[1])
	if args[1] != nil {
		if parent, ok := args[1].(*flavors.Instance); !ok || parent.Type != suiteFlavor {
			slip.TypePanic(s, depth, "parent", args[1], "instance of suite-flavor")
		}
	}
	args = args[2:]
	if value, has := slip.GetArgsKeyValue(args, setupKey); has {
		inst.Let(slip.Symbol("setup"), value)
	}
	if value, has := slip.GetArgsKeyValue(args, teardownKey); has {
		inst.Let(slip.Symbol("teardown"), value)
	}
	_ = inst.Receive(s, ":init", nil, depth)

	return inst
}
