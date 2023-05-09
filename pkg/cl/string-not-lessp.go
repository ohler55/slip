// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringNotLessp{
				stringCompare: stringCompare{
					Function: slip.Function{Name: "string-not-lessp", Args: args},
					compare: func(s1, s2 string) slip.Object {
						if i, diff := compareStringFold(s1, s2); 0 <= diff {
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
			Name:   "string-not-lessp",
			Args:   stringCompareDocArgs,
			Return: "string",
			Text: `__string-not-lessp__ returns the number of matching character if _string1_ is greater
than or equal to _string2_ otherwise _nil_ is returned. Difference in case are ignored.`,
			Examples: []string{
				`(string-not-lessp "abc" "Abb") => 2`,
				`(string-not-lessp "abc" "Abd") => nil`,
			},
		}, &slip.CLPkg)
}

// StringNotLessp represents the string-not-lessp function.
type StringNotLessp struct {
	stringCompare
}
