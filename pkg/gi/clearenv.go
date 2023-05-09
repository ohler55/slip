// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"os"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Clearenv{Function: slip.Function{Name: "clearenv", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "clearenv",
			Args:   []*slip.DocArg{},
			Return: "nil",
			Text:   `__clearenv__ unsets all environment variables.`,
			Examples: []string{
				`(setenv "TEST_VAR" "something")`,
				`(clearenv) => nil`,
				`(getenv "TEST_VAR") => nil`,
			},
		}, &Pkg)
}

// Clearenv represents the clearenv function.
type Clearenv struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Clearenv) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	os.Clearenv()

	return nil
}
