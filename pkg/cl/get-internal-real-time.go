// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.DefConstant(&slip.CLPkg, "internal-time-units-per-second", slip.Fixnum(time.Second),
		"Number of nanoseconds in a second. Internal time units are nanoseconds.")
}

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := GetInternalRealTime{Function: slip.Function{Name: "get-internal-real-time", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "get-internal-real-time",
			Args:   []*slip.DocArg{},
			Return: "fixnum",
			Text:   `__get-internal-real-time__ returns the nanoseconds since January 1, 1970 UTC.`,
			Examples: []string{
				"(get-internal-real-time) => 1685841580000000000",
			},
		}, &slip.CLPkg)
}

// GetInternalRealTime represents the get-internal-real-time function.
type GetInternalRealTime struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *GetInternalRealTime) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 0)

	return slip.Fixnum(time.Now().UnixNano())
}
