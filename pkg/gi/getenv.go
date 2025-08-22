// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"os"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Getenv{Function: slip.Function{Name: "getenv", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "getenv",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "string",
					Text: "The name of the environment variable to get the value of.",
				},
			},
			Return: "string",
			Text:   `__getenv__ returns the environment variable associated with the _name_ or nil is not found.`,
			Examples: []string{
				`(setenv "TEST_VAR" "something")`,
				`(getenv "TEST_VAR") => "something"`,
			},
		}, &Pkg)
}

// Getenv represents the getenv function.
type Getenv struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Getenv) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	vs, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "name", args[0], "string")
	}
	if v := os.Getenv(string(vs)); 0 < len(v) {
		result = slip.String(v)
	}
	return
}
