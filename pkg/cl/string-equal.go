// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringEqual{
				stringCompare: stringCompare{
					Function: slip.Function{Name: "string-equal", Args: args},
					compare: func(s1, s2 string) slip.Object {
						if strings.EqualFold(s1, s2) {
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
			Name:   "string-equal",
			Args:   stringCompareDocArgs,
			Return: "string",
			Text: `__string-equal__ returns true (_t_) if the _string1_ is equal to _string2_
ignoring differences in case. If not equal then _nil_ is returned.`,
			Examples: []string{
				`(string-equal "Abc" "aBc") => t`,
				`(string-equal "abc" "abcd") => nil`,
			},
		}, &slip.CLPkg)
}

// StringEqual represents the string-equal function.
type StringEqual struct {
	stringCompare
}
