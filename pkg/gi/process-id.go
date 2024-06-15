// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"os"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ProcessID{Function: slip.Function{Name: "process-id", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "process-id",
			Args: []*slip.DocArg{},
			Text: `__process-id__ returns the process id of the current process.`,
			Examples: []string{
				`(process-id) => 12345`,
			},
		}, &Pkg)
}

// ProcessID represents the process-id function.
type ProcessID struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ProcessID) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 0)
	return slip.Fixnum(os.Getpid())
}
