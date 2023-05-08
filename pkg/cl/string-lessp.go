// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringLessp{
				stringCompare: stringCompare{
					Function: slip.Function{Name: "string-lessp", Args: args},
					compare: func(s1, s2 string) slip.Object {
						if i, diff := compareStringFold(s1, s2); 0 > diff {
							return slip.Fixnum(i)
						}
						return nil
					},
				},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "string-lessp",
			Args:   stringCompareDocArgs,
			Return: "string",
			Text: `__string-lessp__ returns the number of matching character if _string1_ is less than
_string2_ otherwise _nil_ is returned. Difference in case are ignored.`,
			Examples: []string{
				`(string-lessp "abc" "Abd") => 2`,
				`(string-lessp "abc" "Abb") => nil`,
			},
		}, &slip.CLPkg)
}

// StringLessp represents the string-lessp function.
type StringLessp struct {
	stringCompare
}
