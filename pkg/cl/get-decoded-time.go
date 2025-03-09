// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := GetDecodedTime{Function: slip.Function{Name: "get-decoded-time", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "get-decoded-time",
			Args:   []*slip.DocArg{},
			Return: "values",
			Text: `__get-decoded-time__ returns the values
second, minute, hour, date, month, year, day, daylight-p, zone for the current time.
Note time zones in Common LISP are the reverse of current day time zones. West of UTC
are positive and not negative.`,
			Examples: []string{
				`(get-decoded-time) => 15, 14, 13, 12, 11, 2024, 3, nil, 5`,
			},
		}, &slip.CLPkg)
}

// GetDecodedTime represents the get-decoded-time function.
type GetDecodedTime struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *GetDecodedTime) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 0)

	return decodeTime(time.Now(), false)
}
