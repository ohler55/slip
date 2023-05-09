// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringGreaterp{
				stringCompare: stringCompare{
					Function: slip.Function{Name: "string-greaterp", Args: args},
					compare: func(s1, s2 string) slip.Object {
						if i, diff := compareStringFold(s1, s2); 0 < diff {
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
			Name:   "string-greaterp",
			Args:   stringCompareDocArgs,
			Return: "string",
			Text: `__string-greaterp__ returns the number of matching character if _string1_ is greater than
_string2_ otherwise _nil_ is returned. Difference in case are ignored.`,
			Examples: []string{
				`(string-greaterp "abc" "Abb") => 2`,
				`(string-greaterp "abc" "Abd") => nil`,
			},
		}, &slip.CLPkg)
}

// StringGreaterp represents the string-greaterp function.
type StringGreaterp struct {
	stringCompare
}
