// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"time"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Now{Function: slip.Function{Name: "now", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "now",
			Args: []*slip.DocArg{
				{Name: slip.AmpOptional},
				{
					Name: "location",
					Type: "string",
					Text: "Location for the time. (e.g. America/Toronto)",
				},
			},
			Return: "time",
			Text:   `__now__ returns the current time in the UTC timezone.`,
			Examples: []string{
				`(now) => 2022-07-10T17:29:21.123456789Z`,
			},
		}, &GiPkg)
}

// Now represents the now function.
type Now struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Now) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	var t time.Time
	switch len(args) {
	case 0:
		t = time.Now().UTC()
	case 1:
		t = time.Now().In(getLocArg(args[0]))
	default:
		slip.PanicArgCount(f, 0, 1)
	}
	return slip.Time(t)
}
