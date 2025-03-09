// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := GetUniversalTime{Function: slip.Function{Name: "get-universal-time", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "get-universal-time",
			Args:   []*slip.DocArg{},
			Return: "fixnum",
			Text:   `__get-universal-time__ returns the number of seconds since 1900-01-01T00:00:00Z.`,
			Examples: []string{
				`(get-universal-time) => 3949336106`,
			},
		}, &slip.CLPkg)
}

// GetUniversalTime represents the get-universal-time function.
type GetUniversalTime struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *GetUniversalTime) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 0)

	return slip.Fixnum(time.Now().UTC().Unix() + 2208988800)
}
