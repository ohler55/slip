// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringSame{
				stringCompare: stringCompare{
					Function: slip.Function{Name: "string=", Args: args},
					compare: func(s1, s2 string) slip.Object {
						if s1 == s2 {
							return slip.True
						}
						return nil
					},
				},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "string=",
			Args:   stringCompareDocArgs,
			Return: "string",
			Text: `__string=__ returns true (_t_) if the _string1_ is equal to _string2_
otherwise _nil_ is returned.`,
			Examples: []string{
				`(string= "abc" "abc") => t`,
				`(string= "Abc" "aBc") => nil`,
			},
		}, &slip.CLPkg)
}

// StringSame represents the string= function.
type StringSame struct {
	stringCompare
}
