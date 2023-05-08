// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringNotGreaterp{
				stringCompare: stringCompare{
					Function: slip.Function{Name: "string-not-greaterp", Args: args},
					compare: func(s1, s2 string) slip.Object {
						if i, diff := compareStringFold(s1, s2); 0 >= diff {
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
			Name:   "string-not-greaterp",
			Args:   stringCompareDocArgs,
			Return: "string",
			Text: `__string-not-greaterp__ returns the number of matching character if _string1_ is less
than or equal to _string2_ otherwise _nil_ is returned. Difference in case are ignored.`,
			Examples: []string{
				`(string-not-greaterp "abc" "Abd") => 2`,
				`(string-not-greaterp "abc" "Abb") => nil`,
			},
		}, &slip.CLPkg)
}

// StringNotGreaterp represents the string-not-greaterp function.
type StringNotGreaterp struct {
	stringCompare
}
