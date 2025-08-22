// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := GetInternalRunTime{Function: slip.Function{Name: "get-internal-run-time", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "get-internal-run-time",
			Args:   []*slip.DocArg{},
			Return: "fixnum",
			Text:   `__get-internal-run-time__ returns the nanoseconds since January 1, 1970 UTC.`,
			Examples: []string{
				"(get-internal-run-time) => 1685841580000000000",
			},
		}, &slip.CLPkg)
}

// GetInternalRunTime represents the get-internal-run-time function.
type GetInternalRunTime struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *GetInternalRunTime) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 0)

	return slip.Fixnum(time.Now().UnixNano())
}
