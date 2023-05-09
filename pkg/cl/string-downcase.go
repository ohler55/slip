// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StringDowncase{
				stringModify: stringModify{
					Function: slip.Function{Name: "string-downcase", Args: args},
					modify:   strings.ToLower,
				},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "string-downcase",
			Args:   stringModifyDocArgs,
			Return: "string",
			Text: `__string-downcase__ returns _string_ downcased. All uppercase characters converted
to lowercase in a copy of the _string_.`,
			Examples: []string{
				`(string-downcase "Abc Def") => "abc def"`,
			},
		}, &slip.CLPkg)
}

// StringDowncase represents the string-downcase function.
type StringDowncase struct {
	stringModify
}
