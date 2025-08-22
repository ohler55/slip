// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"os"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Unsetenv{Function: slip.Function{Name: "unsetenv", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "unsetenv",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "string",
					Text: "The name of the environment variable to unset the value of.",
				},
			},
			Return: "nil",
			Text:   `__unsetenv__ unsets the environment variable associated with the _name_.`,
			Examples: []string{
				`(setenv "TEST_VAR" "something")`,
				`(unsetenv "TEST_VAR") => nil`,
				`(getenv "TEST_VAR") => nil`,
			},
		}, &Pkg)
}

// Unsetenv represents the unsetenv function.
type Unsetenv struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Unsetenv) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	vs, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "name", args[0], "string")
	}
	_ = os.Unsetenv(string(vs))

	return nil
}
