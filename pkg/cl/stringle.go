// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringLe{
				stringCompare: stringCompare{
					Function: slip.Function{Name: "string<=", Args: args},
					compare: func(s1, s2 string) slip.Object {
						if s1 > s2 {
							return nil
						}
						ra1 := []rune(s1)
						if s1 == s2 {
							return slip.Fixnum(len(ra1))
						}
						ra2 := []rune(s2)
						for i, r1 := range ra1 {
							if len(ra2) <= i || r1 != ra2[i] {
								return slip.Fixnum(i)
							}
						}
						return slip.Fixnum(len(ra1))
					},
				},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "string<=",
			Args:   stringCompareDocArgs,
			Return: "string",
			Text: `__string<=__ returns the number of matching character if _string1_ is
less than or equal to _string2_ otherwise _nil_ is returned.`,
			Examples: []string{
				`(string<= "abc" "abd") => 2`,
				`(string<= "abc" "abb") => nil`,
				`(string<= "abc" "abc") => 3`,
			},
		}, &slip.CLPkg)
}

// StringLe represents the string<= function.
type StringLe struct {
	stringCompare
}
