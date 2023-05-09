// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringGe{
				stringCompare: stringCompare{
					Function: slip.Function{Name: "string>=", Args: args},
					compare: func(s1, s2 string) slip.Object {
						if s2 > s1 {
							return nil
						}
						ra1 := []rune(s1)
						if s1 == s2 {
							return slip.Fixnum(len(ra1))
						}
						ra2 := []rune(s2)
						for i, r2 := range ra2 {
							if len(ra1) <= i || r2 != ra1[i] {
								return slip.Fixnum(i)
							}
						}
						return slip.Fixnum(len(ra2))
					},
				},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "string>=",
			Args:   stringCompareDocArgs,
			Return: "string",
			Text: `__string>=__ returns the number of matching character if _string1_ is greater
than or equal to _string2_ otherwise _nil_ is returned.`,
			Examples: []string{
				`(string>= "abc" "abb") => 2`,
				`(string>= "abc" "abc") => 3`,
				`(string>= "abc" "abd") => nil`,
			},
		}, &slip.CLPkg)
}

// StringGe represents the string>= function.
type StringGe struct {
	stringCompare
}
