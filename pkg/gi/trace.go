// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Trace{Function: slip.Function{Name: "trace", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "trace",
			Args: []*slip.DocArg{
				{
					Name: "on",
					Type: "boolean",
					Text: "If not nil then trun tracing on else turn tracing off.",
				},
			},
			Text: `__trace__ turns tracing on or off.`,
			Examples: []string{
				`(trace t) ;; tracing on`,
				`(trace nil) ;; tracing off`,
			},
		}, &GiPkg)
}

// Trace represents the trace function.
type Trace struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Trace) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	slip.Trace(args[0] != nil)

	return slip.Novalue
}
