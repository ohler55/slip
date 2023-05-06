// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := NstringUpcase{
				stringModify: stringModify{
					Function: slip.Function{Name: "nstring-upcase", Args: args},
					modify:   strings.ToUpper,
				},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "nstring-upcase",
			Args:   stringModifyDocArgs,
			Return: "string",
			Text: `__nstring-upcase__ returns _string_ upcased. All lowercase characters converted
to uppercase in a copy of the _string_.`,
			Examples: []string{
				`(string-upcase "Abc Def") => "ABC DEF"`,
			},
		}, &slip.CLPkg)
}

// NstringUpcase represents the string-upcase function.
type NstringUpcase struct {
	stringModify
}
