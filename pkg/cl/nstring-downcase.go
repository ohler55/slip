// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := NstringDowncase{
				stringModify: stringModify{
					Function: slip.Function{Name: "nstring-downcase", Args: args},
					modify:   strings.ToLower,
				},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "nstring-downcase",
			Args:   stringModifyDocArgs,
			Return: "string",
			Text: `__nstring-downcase__ returns _string_ downcased. All uppercase characters converted
to lowercase in a copy of the _string_.`,
			Examples: []string{
				`(nstring-downcase "Abc Def") => "abc def"`,
			},
		}, &slip.CLPkg)
}

// NstringDowncase represents the string-downcase function.
type NstringDowncase struct {
	stringModify
}
