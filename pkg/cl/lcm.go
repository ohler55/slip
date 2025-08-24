// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Lcm{Function: slip.Function{Name: "lcm", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "lcm",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{Name: "integers", Type: "fixnum"},
			},
			Return: "fixnum",
			Text:   `__lcm__ returns the least common multiple of _integers_.`,
			Examples: []string{
				"(lcm) => 1",
				"(lcm -3) => 3",
				"(lcm 70 42) => 210",
			},
		}, &slip.CLPkg)
}

// Lcm represents the lcm function.
type Lcm struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Lcm) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	z := slip.Fixnum(1)
	for i, a := range args {
		num, ok := a.(slip.Fixnum)
		if !ok {
			slip.TypePanic(s, depth, "integers", a, "fixnum")
		}
		switch {
		case num == 0:
			return num
		case num < 0:
			num = -num
		}
		if i == 0 { // first one
			z = num
		} else {
			z = z * num / gcd(z, num)
		}
	}
	return z
}
