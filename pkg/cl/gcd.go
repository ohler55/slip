// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Gcd{Function: slip.Function{Name: "gcd", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "gcd",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{Name: "integers", Type: "fixnum"},
			},
			Return: "fixnum",
			Text:   `__gcd__ returns the greatest common divisor of _integers_.`,
			Examples: []string{
				"(gcd) => 0",
				"(gcd 3) => 3",
				"(gcd 70 42) => 7",
			},
		}, &slip.CLPkg)
}

// Gcd represents the gcd function.
type Gcd struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Gcd) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	z := slip.Fixnum(0)
	for i, a := range args {
		num, ok := a.(slip.Fixnum)
		if !ok {
			slip.PanicType("integers", a, "fixnum")
		}
		if num < 0 {
			num = -num
		}
		if i == 0 { // first one
			z = num
		} else {
			z = gcd(z, num)
		}
	}
	return z
}

func gcd(x, y slip.Fixnum) slip.Fixnum {
	for y != 0 {
		x, y = y, x%y
	}
	return x
}
