// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringNotEqual{
				stringCompare: stringCompare{
					Function: slip.Function{Name: "string-not-equal", Args: args},
					compare: func(s1, s2 string) slip.Object {
						if i, diff := compareStringFold(s1, s2); diff != 0 {
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
			Name:   "string-not-equal",
			Args:   stringCompareDocArgs,
			Return: "string",
			Text: `__string-not-equal__ returns the number of matching character if _string1_ is less than
_string2_ otherwise _nil_ is returned. Difference in case are ignored.`,
			Examples: []string{
				`(string-not-equal "abc" "Abd") => 2`,
				`(string-not-equal "abc" "Abb") => nil`,
			},
		}, &slip.CLPkg)
}

// StringNotEqual represents the string-not-equal function.
type StringNotEqual struct {
	stringCompare
}
