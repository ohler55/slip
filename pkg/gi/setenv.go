// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"os"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Setenv{Function: slip.Function{Name: "setenv", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "setenv",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "string",
					Text: "The name of the environment variable to set the value of.",
				},
				{
					Name: "value",
					Type: "string",
					Text: "The value to set the _name_ environment variable to.",
				},
			},
			Return: "nil",
			Text:   `__setenv__ sets the environment variable _name_ to _value_.`,
			Examples: []string{
				`(setenv "TEST_VAR" "something")`,
				`(getenv "TEST_VAR") => "something"`,
			},
		}, &Pkg)
}

// Setenv represents the setenv function.
type Setenv struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Setenv) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	var (
		name  string
		value string
	)
	if vs, ok := args[0].(slip.String); ok {
		name = string(vs)
	} else {
		slip.TypePanic(s, depth, "name", args[0], "string")
	}
	if vs, ok := args[1].(slip.String); ok {
		value = string(vs)
	} else {
		slip.TypePanic(s, depth, "value", args[1], "string")
	}
	if err := os.Setenv(name, value); err != nil {
		slip.ErrorPanic(s, depth, "setenv of %s to %s failed. %s", name, value, err)
	}
	return nil
}
